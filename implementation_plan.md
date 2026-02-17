# Implementation Plan: `oq` Agent-First MVP

Source PRD: `/Users/sergeykostyaev/ocaml/oq/docs/PRD-oq-v1.md` (Final v1.0, 2026-02-17)

## Agent-First Principles (applies to all milestones)

1. Agent-oriented defaults:
   outputs should be concise, deterministic, and suitable for chained tool calls.
2. Stable contracts:
   keep strict selector semantics, canonical error/warning templates, and exit-code guarantees.
3. Token efficiency by design:
   prioritize structure-first and scoped extraction paths over full-text dumps.
4. Automatable behavior:
   avoid hidden runtime dependencies and preserve reproducible results with `--now` and `--tz`.
5. Docs for agents:
   provide ready-to-use query playbooks and failure-recovery guidance for agent loops.

## 0. Repository Bootstrap (Day 0, 0.5-1 day) [DONE]

1. Initialize git repository:
   `git init -b main`.
2. Add repository hygiene files:
   `.gitignore` (OCaml/dune artifacts), `.editorconfig` (optional but recommended).
3. Add MIT license:
   create `LICENSE` with MIT text and project copyright.
4. Create initial commit:
   `git add . && git commit -m "chore: bootstrap oq repository"`.
5. Authenticate and verify GitHub CLI:
   `gh auth login` and `gh auth status`.
6. Create GitHub repository under personal account with name `oq` via `gh`:
   `gh repo create oq --source=. --remote=origin --push`.
7. Definition of Done:
   local repo is initialized, `origin` points to personal GitHub repo `oq`, initial commit is pushed, MIT license is present.

## 1. Project Bootstrap + Agent Contract (M0, 1-2 days)

1. [DONE] Initialize OCaml project skeleton (`dune`, `opam`, `bin/oq.ml`, `lib/*`, `test/*`).
2. [DONE] Add core dependencies: `base`, `core`, `stdio`, `ppx_jane`, `cmdliner`, `re2`.
3. [DONE] Lock CLI contracts with smoke tests:
   exit codes (`0`, `1`, `2`, `3`), canonical `Error:`/`Warning:` templates, deterministic ordering rules.
4. [DONE] Add "agent contract" doc:
   minimal stable command patterns, expected outputs, and retry behavior for common failures.
5. [DONE] Definition of Done:
   `oq` builds, runs, and basic error classes map to required exit codes.

## 2. Fixtures and Spec Tests for Agent Workflows (M0, 2-3 days)

1. [DONE] Build `.org` fixture corpus for all locked decisions:
   duplicate headings, TODO workflows, planning lines, drawers, blocks, links, tables.
2. [DONE] Add golden tests for exact diagnostics and agent stability:
   ambiguity template and parse warning template.
3. [DONE] Add end-to-end fixture scenarios that mirror agent tasks:
   "find section -> narrow -> extract text", "directory search -> filter -> select heading", "recover from ambiguity error".
4. [DONE] Definition of Done:
   fixture set covers MVP semantics from PRD and is used by automated tests.

## 3. Org Parser and Indexing (M1, 5-7 days)

1. Implement focused custom Org subset parser (PRD Option A).
2. Build indexes for headings, section ranges, TODO state, planning metadata, properties, blocks, links, tables.
3. Normalize parse-failure reason codes:
   `syntax_error`, `unsupported_construct`, `invalid_utf8`, `internal_parser_error`.
4. Ensure all indexed entities keep precise spans and source references needed by agents for follow-up queries.
5. Definition of Done:
   fixtures parse reliably with line-accurate spans and stable reason mapping.

## 4. Query Language Parser (M2, 4-5 days)

1. Implement canonical grammar from PRD with precedence:
   `comparison` > `and` > `or`.
2. Support postfix chaining:
   indexing/slicing plus field access (`.headings[0].title`).
3. Enforce grammar/validation constraints:
   reject bare `| [0]`, reject bare `| length`, validate regex pattern-strings `"/pattern/flags"` with flags `i,m,s`.
4. Tighten diagnostics to support agent self-correction:
   unknown selector, wrong type, malformed regex, bad postfix usage.
5. Definition of Done:
   parser tests for precedence, postfix binding, and negative grammar scenarios pass.

## 5. Evaluator: Core Selectors and Functions (M2, 5-6 days)

1. Implement selectors:
   `.tree`, `.headings`, `.section`, `.section_contains`, `.sections`, `.search`, `.text`, `.length`.
2. Implement pipeline and functions:
   `filter(predicate)`, `map(...)`, comparisons, `contains`, `startswith`, `endswith`.
3. Implement `.section("title")` ambiguity behavior with disambiguation via `.section("title", start:end)`.
4. Optimize selector execution order for agent loops:
   structure-first queries should be fast and avoid unnecessary full-content materialization.
5. Definition of Done:
   golden tests for core queries and canonical errors pass.

## 6. Org-Specific Selectors and Date Semantics (M3, 5-6 days)

1. Implement selectors:
   `.todos`, `.done`, `.properties`, `.property("KEY")`, `.tags`, `.code("lang")`, `.links`, `.tables`.
2. Implement planning selectors:
   `.scheduled`, `.deadline`, `.closed` with ranges `today`, `tomorrow`, `yesterday`, `this_week`, `next_7d`, `overdue`.
3. Implement deterministic time options:
   `--now <RFC3339>` and `--tz <IANA_TZ>` with offset mismatch validation.
4. Validate agent-oriented date reproducibility:
   same query with fixed `--now`/`--tz` must produce byte-stable output.
5. Definition of Done:
   deterministic boundary tests around midnight/week start and overdue behavior pass.

## 7. Directory Mode and Deterministic Ordering (M3-M4, 4-5 days)

1. Implement traversal rules:
   include only `.org`, skip hidden paths, skip symlinks.
2. Implement parse summary counters and invariant:
   `candidate_org = parsed_ok + parse_failed`.
3. Implement canonical multi-file ordering:
   normalized relative path (`\` -> `/`, Unicode NFC), then `start_line`, then `end_line`.
4. Implement parse-failure behavior:
   graceful degrade by default, `--strict` returns exit code `3` on any parse failure.
5. Add large-corpus regression tests to keep agent behavior stable under scale (hundreds of files).
6. Definition of Done:
   integration tests for zero-candidate, partial-failure, all-failed, and strict-mode paths pass.

## 8. Hardening, Performance, and Release (M4, 3-5 days)

1. Profile and optimize hot paths (`.search`, directory traversal, rendering).
2. Set CI gates:
   lint + unit + golden + integration + benchmark checks.
3. Add agent E2E test suite:
   scripted scenarios that emulate iterative agent usage and validate stable outputs between runs.
4. Build release flow:
   GitHub binaries first, then `opam`, then Homebrew.
5. Publish "Agent Playbook":
   compact cookbook of high-signal queries, troubleshooting flow, and deterministic run recommendations.
6. Standardize GitHub operations through `gh` CLI:
   issue/PR triage, release creation (`gh release create`), and CI status checks (`gh run list`, `gh run view`).
7. Definition of Done:
   MVP meets PRD success metrics for latency, reliability, and deterministic output.

## Critical Path

1. Parser/index correctness.
2. Query grammar + evaluator correctness.
3. Date semantics + directory semantics stability.
4. Test hardening and release packaging.

## Delivery Estimate

1. End-to-end MVP target:
   4-6 weeks of focused development.

## Agent-First Success Addendum

1. Agent task completion:
   >= 90% of scripted agent scenarios complete without manual fallback commands.
2. Agent token efficiency:
   maintain target >= 60% token reduction versus naive full-file reads.
3. Determinism under reruns:
   repeated runs on unchanged inputs produce identical output ordering and diagnostics.
