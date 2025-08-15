import asyncio
import base64
import json
from typing import Tuple

import pytest

import emacs_mcp_server as srv
def test_base64_json_malformed(monkeypatch):
    # Force malformed base64 to exercise error path
    def fake_eval(expr: str, timeout: float = 5.0):
        return ('"not_base64"', "", 0)

    monkeypatch.setattr(srv, "_run_emacsclient_eval", fake_eval)
    try:
        # Test with evaluate which uses sync path for smoke test
        srv._eval_b64_json("(list (cons 'test \"value\"))")
        assert False, "Expected EmacsError on malformed base64"
    except srv.EmacsError as e:
        assert "Failed to decode" in str(e)


@pytest.mark.asyncio
async def test_emacsclient_timeout(monkeypatch):
    async def fake_run(expr: str, timeout: float | None = None) -> Tuple[str, str, int]:
        raise srv.EmacsError("emacsclient request timed out")

    monkeypatch.setattr(srv, "_run_emacsclient_eval_async", fake_run)

    resp = await srv.emacs_get_visible_text.fn()
    assert resp["success"] is False and "timed out" in resp["error"].lower()


@pytest.mark.asyncio
async def test_evaluate_async_success(monkeypatch):
    async def fake_run(expr: str, timeout: float | None = None) -> Tuple[str, str, int]:
        return ("42", "", 0)

    monkeypatch.setattr(srv, "_run_emacsclient_eval_async", fake_run)

    result = await srv.evaluate_async("(+ 40 2)")
    assert result == "42"


@pytest.mark.asyncio
async def test_tool_emacs_eval_error_envelope(monkeypatch):
    async def fake_run(expr: str, timeout: float | None = None) -> Tuple[str, str, int]:
        return ("", "Symbol undefined", 1)

    monkeypatch.setattr(srv, "_run_emacsclient_eval_async", fake_run)

    resp = await srv.emacs_eval.fn("(undefined)")
    assert resp["success"] is False
    assert "error" in resp


@pytest.mark.asyncio
async def test_visible_text_tool_success(monkeypatch):
    async def fake_eval_b64(el: str, timeout: float | None = None):
        # Ensure window targeting wrappers are present
        assert "with-selected-window" in el
        return {"text": "Hello", "start": 1, "end": 6}

    monkeypatch.setattr(srv, "_eval_b64_json_async", fake_eval_b64)

    resp = await srv.emacs_get_visible_text.fn()
    assert resp["success"] is True
    assert resp["text"] == "Hello"
    assert resp["start"] == 1 and resp["end"] == 6


@pytest.mark.asyncio
async def test_context_tool_success(monkeypatch):
    async def fake_eval_b64(el: str, timeout: float | None = None):
        # Context path should also include window-aware calls in Elisp
        assert "window-start" in el or "window-end" in el
        return {"buffer_name": "test.py", "project_root": "/tmp/proj"}

    monkeypatch.setattr(srv, "_eval_b64_json_async", fake_eval_b64)

    resp = await srv.emacs_get_context.fn()
    assert resp["success"] is True
    assert resp["context"]["buffer_name"] == "test.py"
    assert resp["context"]["project_root"] == "/tmp/proj"
