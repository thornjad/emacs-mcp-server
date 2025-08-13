import asyncio
import base64
import json
from typing import Tuple

import pytest

import emacs_mcp_server as srv


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

    resp = await srv.emacs_eval("(undefined)")
    assert resp["success"] is False
    assert "error" in resp


@pytest.mark.asyncio
async def test_visible_text_tool_success(monkeypatch):
    async def fake_eval_b64(el: str, timeout: float | None = None):
        return {"text": "Hello", "start": 1, "end": 6}

    monkeypatch.setattr(srv, "_eval_b64_json_async", fake_eval_b64)

    resp = await srv.emacs_get_visible_text()
    assert resp["success"] is True
    assert resp["text"] == "Hello"
    assert resp["start"] == 1 and resp["end"] == 6


@pytest.mark.asyncio
async def test_context_tool_success(monkeypatch):
    async def fake_eval_b64(el: str, timeout: float | None = None):
        return {"buffer_name": "test.py", "project_root": "/tmp/proj"}

    monkeypatch.setattr(srv, "_eval_b64_json_async", fake_eval_b64)

    resp = await srv.emacs_get_context()
    assert resp["success"] is True
    assert resp["context"]["buffer_name"] == "test.py"
    assert resp["context"]["project_root"] == "/tmp/proj"
