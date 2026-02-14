# Kanban Tool — Spike Spec & Language Evaluation Framework

## 1. Vision

A **text-first, methodology-agnostic** work tracking tool. Think `ledger-cli` for task management. The text format is the source of truth — human-readable, git-friendly, agent-friendly. A rendering layer (TUI, web preview, PlantUML-style board visualization) is a separate concern that consumes the same format.

**Non-goals:** Enforcing Scrum, sprint boundaries, velocity tracking, or any specific methodology. The tool provides *mechanics*, not *opinions*.

---

## 2. Core Domain Model

### Item
The atomic unit. Deliberately flat — no deep hierarchies.

| Field        | Required | Notes                                                    |
|------------- |----------|----------------------------------------------------------|
| `id`         | yes      | Short, human-typeable. Auto-generated or user-assigned.  |
| `title`      | yes      | One-liner.                                               |
| `type`       | yes      | `task`, `story`, `spike`, `bug`. Extensible (user-defined types). |
| `status`     | yes      | Current lane. String matching a board lane name.         |
| `tags`       | no       | Freeform labels. Flat list.                              |
| `desc`       | no       | Multi-line description / acceptance criteria.            |
| `deps`       | no       | List of item IDs this item depends on.                   |
| `assignee`   | no       | Who's working on it (human or agent name/id).            |
| `priority`   | no       | User-defined scale (e.g., `p0`–`p3`, or `high/med/low`).|
| `created`    | auto     | Timestamp.                                               |
| `modified`   | auto     | Timestamp.                                               |
| `history`    | auto     | Append-only log of state transitions.                    |

### Board
A named configuration of lanes.

| Field    | Notes                                                         |
|----------|---------------------------------------------------------------|
| `name`   | Board identifier.                                             |
| `lanes`  | Ordered list of lane names representing workflow stages.      |

Default board: `backlog → doing → review → done`

### Swimlanes (optional/future)
Horizontal groupings by type, assignee, priority, or tag. A *view* concern, not a data concern — derived from item fields, not stored separately.

---

## 3. Text Format

The canonical representation. Must satisfy:

- **Human-writable** in a plain text editor
- **Diffable** in git (line-oriented where possible)
- **Parseable** without ambiguity
- **Agent-manipulable** with minimal token overhead

### Candidate format (strawman — each language may propose alternatives)

```
# Board: default
# Lanes: backlog, doing, review, done

[KAN-001] spike: Evaluate Smalltalk for agent coding
  status: backlog
  tags: language-spike, research
  priority: p1
  desc: |
    Run the kanban tool spike in Pharo Smalltalk.
    Measure token economy, composability, error recovery.
  created: 2025-02-14
  
[KAN-002] task: Set up Pharo Smalltalk environment
  status: backlog
  tags: language-spike, infra
  deps: KAN-001
  created: 2025-02-14
```

**Note:** Part of the spike is seeing whether each language proposes a *better native format*. Lisp will want s-expressions. Forth might want a stack-oriented DSL. That's signal, not noise.

---

## 4. Operations (CLI-first)

```
kb add <type> "<title>" [--tag=X] [--priority=X] [--lane=X]
kb move <id> <lane>
kb edit <id> [--title=X] [--desc=X] [--tag+=X] [--tag-=X]
kb show <id>                    # full detail
kb ls [--lane=X] [--tag=X] [--type=X] [--assignee=X]
kb board                        # render board view (text)
kb log <id>                     # show history
kb archive <id>                 # move to archive, out of active board
kb lanes add|remove|reorder ... # board configuration
```

### Agent-specific operations (stretch goal)
```
kb next [--type=X] [--tag=X]   # "what should I work on?" — respects deps & priority
kb claim <id> <agent-name>     # agent self-assigns
kb note <id> "<text>"          # append a work note (agent or human)
```

---

## 5. Implementation Guidelines

### What to build

1. **Parser** — read the text format (or propose a language-native format) into internal representation
2. **Core operations** — the CLI commands above (at minimum: add, move, ls, board, show)
3. **Serializer** — write back to text format
4. **Extension exercise** — after core works, add a `blocked` status that automatically applies when a dependency isn't `done`, and a `kb blocked` command to list all blocked items with reasons

### Context

This is one of 6 parallel implementations (GNU Smalltalk, GForth, Racket, Lua, Go, Python) of the same spec. The goal is to evaluate which languages give AI agents the best leverage for building tools.

You (Claude) are the developer. Cedar is the product owner / reviewer. Standard library only where possible — library pain is signal we're measuring.

### Format freedom

The strawman text format in section 3 is a starting point. If your language suggests a more natural representation, **propose it**. The format should feel native to the language, not forced. This is part of what we're evaluating.

---

## 6. Evaluation Criteria

### A. Quantitative

| Metric                         | How to measure                                              |
|--------------------------------|-------------------------------------------------------------|
| **Token economy (solution)**   | Token count of the final working artifact                   |
| **Token economy (process)**    | Total tokens consumed across all prompts to reach working state |
| **Lines of code**              | Raw LOC of final solution                                   |
| **Abstraction density**        | Ratio of reusable abstractions (functions/words/methods) to total code |
| **Extension cost**             | Tokens/LOC needed for the extension exercise                |
| **Error recovery cost**        | Tokens consumed in debugging/fix cycles                     |
| **Iteration count**            | Number of prompt-response rounds to reach working state     |

### B. Qualitative (introspected + reviewer)

| Dimension                      | Questions                                                   |
|--------------------------------|-------------------------------------------------------------|
| **Naturalness of fit**         | Does the language's paradigm match the domain? Does the solution feel *native* or forced? |
| **Composability**              | Did the agent build things that snap together, or monolithic blocks? |
| **Readability (by agent)**     | Can the agent reason about its own code in follow-up prompts? |
| **Readability (by human)**     | Can Cedar read and understand the solution without deep language expertise? |
| **Format expressiveness**      | Did the language suggest a better data format than the strawman? |
| **Surprise factor**            | Did anything unexpected emerge — good or bad?               |
| **Library pain**               | How much did missing libraries hurt?                        |
| **Self-extension quality**     | When extending, did the agent *leverage* existing abstractions or duplicate? |

### C. Agent-readiness

| Dimension                      | Questions                                                   |
|--------------------------------|-------------------------------------------------------------|
| **Context pressure**           | As the codebase grows, how quickly does the agent "lose the thread"? |
| **Toolability**                | How naturally does the solution expose itself as a tool for other agents? |
| **Inspectability**             | Can an agent introspect the running system?                 |
| **Grammar-in-context cost**    | How many tokens to give the agent "enough" language knowledge to be competent? |
