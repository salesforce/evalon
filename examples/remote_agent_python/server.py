# /// script
# requires-python = ">=3.10"
# dependencies = [
#   "fastapi",
#   "uvicorn",
# ]
# ///
"""Minimal reference server for the Evalon remote-agent protocol.

Run:
    uv run server.py

Then point a scenario's evaluated participant at it:
    participants:
      agent:
        type: evaluated
        endpoint: http://localhost:8080
"""

from fastapi import FastAPI
from pydantic import BaseModel
from typing import Any, Literal, Optional, Union
import uvicorn

app = FastAPI()


class Turn(BaseModel):
    type: Literal["turn"]
    conversation: str
    sender: str
    content: str


class ToolCall(BaseModel):
    tool_name: str
    arguments: dict[str, Any]


class ToolResult(BaseModel):
    tool_name: str
    arguments: dict[str, Any]
    result: Any
    error: Optional[str] = None


class ToolInteraction(BaseModel):
    call: ToolCall
    result: ToolResult


class ToolUse(BaseModel):
    type: Literal["tool_use"]
    conversation: str
    interaction: ToolInteraction


HistoryEntry = Union[Turn, ToolUse]


class Event(BaseModel):
    name: str
    data: dict[str, Any] = {}


class StepRequest(BaseModel):
    protocol_version: str
    history: list[HistoryEntry]
    events: list[Event]
    respond_in: str


@app.post("/v1/step")
def step(req: StepRequest):
    # Look at req.history (a mix of Turn and ToolUse entries) and req.events.
    # Decide what to say in req.respond_in. Run any tools yourself; report them in tool_trace.
    last_user_message = next(
        (
            e.content
            for e in reversed(req.history)
            if isinstance(e, Turn) and e.sender != "agent"
        ),
        "(no prior message)",
    )
    return {
        "action": {
            "type": "send",
            "message": {
                "sender": "agent",
                "content": f"Echo from remote agent: {last_user_message}",
            },
            "tool_trace": [],
        }
    }


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8080)
