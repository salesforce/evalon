# Evalon Core

Actor-based agent evaluation framework with simulation and LLM-as-judge scoring. Built with Scala 3 and Apache Pekko.

Evalon simulates multi-party conversations between participants, then evaluates an agent's performance against behavioral criteria. Each participant and event source runs as an independent actor, with the ScenarioRunner orchestrating message routing and transcript recording.

## Setup

1. Install dependencies:

```bash
sbt compile
```

2. Configure API credentials:

```bash
cp .env.example .env
# Edit .env with your credentials
```

Environment variables:
- `ANTHROPIC_BASE_URL` — API endpoint (auto-strips `/bedrock` suffix)
- `ANTHROPIC_API_KEY` — auth token
- `EVALON_CA_CERTS` — path to CA cert bundle (for corporate proxies / custom CA bundles)

3. Load environment and run:

```bash
set -a && source .env && set +a && sbt run
```

## Running scenarios

```bash
# Flight rescheduling (direct topology: user <-> agent)
sbt "run scenarios/reschedule_flight.yaml"

# Hotel cancellation (assist topology: user <-> rep, agent observes + assists)
sbt "run scenarios/hotel_cancellation_assist.yaml"

# Payment activation (direct, 3 tools)
sbt "run scenarios/payment_activation.yaml"

# Complex payment issue (direct, 14 tools, 30 turns)
sbt "run scenarios/comprehensive_payment_issue.yaml"

# Order refund workflow (direct, 8 tools)
sbt "run scenarios/order_refund.yaml"
```

## Architecture

### Actor system

| Actor | Role |
|-------|------|
| `ScenarioRunner` | Orchestrator — spawns participants, routes messages to direct participants, records transcript |
| `Participant` | Shared protocol (`ReceiveMessage`, `ReceiveEvents`) for all participant actors |
| `SimulatedParticipant` | Self-driven LLM actor with per-conversation history and cross-conversation context |
| `EvaluatedAgent` | Wraps the `Agent` trait, bridges actor messages to `Future`-based calls |
| `EventSourceActor` | Observes simulation traffic, uses LLM reasoning to emit derived events |

### Core modules

- `model/` — Domain types: `Message`, `Action`, `Event`, `HistoryEntry`, `Transcript`, `Scenario`, `EvalResult`
- `actor/` — Pekko typed actors for simulation participants and orchestration
- `agent/` — `Agent` trait, `ClaudeAgent` (in-process Claude with tool-use loop), and `RemoteAgent` (HTTP proxy for non-JVM agents)
- `tool/` — `Tool` trait and mock implementations (flights, hotels, payments, refunds)
- `llm/` — sttp-based Anthropic Messages API client with circe codecs
- `scenario/` — YAML scenario loader (circe-yaml)
- `eval/` — LLM-as-judge evaluator
- `output/` — Colored transcript printer

### Concepts

- **Participants** — `simulated` (LLM-driven), `evaluated` (agent under test), or `custom` (user code)
- **Conversations** — named channels between participants, with an optional initiator
- **Observations** — allow a participant to observe messages in conversations it's not directly part of (currently only `EvaluatedAgent` reacts to observed events; `SimulatedParticipant` ignores them — support will be added when a scenario needs it)
- **Event sources** — actors that observe all simulation traffic and emit derived events (⚠️ experimental: `custom` source type is not wired up, dedup of repeated emissions is not handled, and no shipped scenario exercises this path yet)
- **Transcript** — immutable ordered log of all messages, tool calls, tool results, and events

### Evaluating remote agents

Evalon can evaluate agent implementations written in any language by proxying the `Agent` trait over HTTP. To use a remote agent, set an `endpoint:` on the evaluated participant in the scenario YAML — Evalon will construct a `RemoteAgent` instead of `ClaudeAgent`.

The agent server implements one endpoint, `POST /v1/step`. Each request carries the full conversation history (turns and prior tool uses), accumulated system events, and the conversation the response should target. Evalon owns history; the server is stateless. The server runs its own tools and reports the trace in the response.

Wire format (request):
```json
{
  "protocol_version": "1",
  "history": [
    {"type": "turn", "conversation": "support_chat", "sender": "end_user", "content": "..."},
    {"type": "tool_use", "conversation": "agent_assist", "interaction": {"call": {...}, "result": {...}}}
  ],
  "events": [{"name": "case_created", "data": {...}}],
  "respond_in": "support_chat"
}
```

Wire format (response):
```json
{"action": {"type": "send", "message": {"sender": "agent", "content": "..."}, "tool_trace": []}}
```
Or `{"action": {"type": "end"}}` to end the conversation.

A minimal Python server (FastAPI, ~70 lines) lives at [`examples/remote_agent_python/server.py`](examples/remote_agent_python/server.py). With [`uv`](https://github.com/astral-sh/uv) installed:

```bash
uv run examples/remote_agent_python/server.py
# then in another terminal:
sbt "run scenarios/reschedule_flight.yaml"  # with endpoint: uncommented in the YAML
```

Evalon retries 429/5xx responses with exponential backoff (3 retries, 1s base). Other errors fail the run.

### Simulation flow

1. ScenarioRunner spawns all participant and event source actors
2. Conversations with an `initiated_by` participant kick off; that participant receives a start message
3. Participants are self-driven: when a message arrives and the actor is idle, it generates a response asynchronously. Messages arriving during generation are accumulated and processed afterward
4. Messages are recorded in the transcript and delivered to direct participants (those in the conversation's `between` list)
5. Observer notifications deliver new messages as events (`ReceiveEvents`) to observing participants
6. Event sources receive all traffic and may emit additional events
7. Simulation ends when any participant signals `[END]` or `max_turns` is reached
8. The evaluator scores the transcript against per-criterion behavioral checks

### Scenario YAML format

```yaml
name: reschedule_flight
description: >
  A customer contacts support because their flight has been delayed.

participants:
  end_user:
    type: simulated
    persona: You are Jane Doe, a frustrated customer...
    goal: Get rebooked on the next available flight.
  agent:
    type: evaluated
    # Optional: point at an HTTP server speaking the remote-agent protocol (see
    # examples/remote_agent_python/server.py). Without this, evaluation uses ClaudeAgent.
    # endpoint: http://localhost:8080

conversations:
  - name: support_chat
    between: [end_user, agent]
    initiated_by: end_user

observations:
  - participant: agent
    observes: [support_chat]

event_sources:
  case_events:
    type: simulated
    description: A case management system...
    emits:
      - event: case_created
        schema: { case_id: string, priority: string }

context:
  flights:
    AA123: { flight_number: AA123, status: delayed, ... }

max_turns: 10

eval_criteria:
  - description: "Agent looked up the customer's flight status"
  - description: "Agent rebooked the customer"
```
