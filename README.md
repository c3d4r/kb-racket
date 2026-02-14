# kb-racket

Racket implementation of the `kb` kanban tool.

**Part of the [Language Choice as Superpower](https://github.com/cedar/language_choice_as_superpower) research spike.**

## What This Is

A text-first, CLI kanban board tool. Same spec implemented in 6 languages to evaluate which languages give AI agents the best leverage. Racket tests the hypothesis that a language-oriented programming system — where you build DSLs as naturally as functions — gives agents superior expressiveness.

## The Spec

See [SPEC.md](./SPEC.md) for the full specification. Key points:

- **Text-first:** The data file is the source of truth — human-readable, git-diffable, agent-friendly
- **CLI interface:** `kb add`, `kb move`, `kb ls`, `kb board`, `kb show`, etc.
- **Methodology-agnostic:** Lanes and flow, not Scrum opinions
- **Format freedom:** If Racket suggests a more natural data format (s-expressions? a `#lang kb`?), propose it
- **Extension exercise:** After core works, add `blocked` status (auto-derived from deps) and `kb blocked` command

## Runtime

- **Language:** Racket
- **Dependencies:** Standard library only
- **Run:** `racket kb.rkt <command> [args]`

## Why Racket

- Language-oriented programming — build a DSL for the domain, not just functions
- Homoiconicity (code as data) — agents can inspect, rewrite, generate programs
- Macro system for principled metaprogramming
- `#lang` mechanism could make the data file itself a Racket program
- Clean, modern Scheme with batteries included

## Status

- [ ] Core: parser, serializer, internal model
- [ ] CLI: add, move, ls, board, show
- [ ] Extension: blocked status + kb blocked command
- [ ] Evaluation notes captured
