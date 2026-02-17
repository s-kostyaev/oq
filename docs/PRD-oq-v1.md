# PRD: `oq` (Org Query CLI)

Status: Final v1.0  
Owner: Sergey + Codex  
Last updated: 2026-02-17
Original project reference (`mq`): https://github.com/muqsitnawaz/mq

## 0. Decisions (Locked)

1. Query language strategy: **Option B**
   Keep `mq`-style core syntax compatibility, then add Org-specific selectors and entities.
2. Section lookup semantics:
   `.section("title")` uses exact heading match.
   Matching is case-sensitive in MVP.
   If exactly one heading matches, return that section.
   If multiple headings match, return an ambiguity error with candidate line ranges.
   Disambiguation is supported via `.section("title", start:end)`.
3. Substring section lookup:
   provided via separate selector `.section_contains("term")`.
4. Regex lookup:
   allowed via `.search(...)` regex mode.
5. Binary name:
   use `oq`.
6. Input formats in MVP:
   only `.org`.
7. Output format in MVP:
   text-only; `--json` is out of scope for v1.
8. TODO workflow in MVP:
   configurable from file directives (`#+TODO`), with fallback to `TODO | DONE` when absent.
9. Date filtering in MVP:
   include relative filters from day one (not only raw date extraction).
   Semantics are aligned to Org mode defaults for relative day/week windows and overdue behavior.
10. `.links` and `.tables` scope:
   include in v1 (not deferred to Phase 2).
11. Distribution order:
   `GitHub release binaries` -> `opam` -> `Homebrew`.
12. Dependency licensing policy for v1:
   permissive-only dependencies (MIT/ISC/BSD/Apache); no AGPL/LGPL copyleft deps in default build.
13. Query filtering function in MVP:
   use `filter(predicate)` only.
   `select(predicate)` is not supported.
14. Exit code contract in MVP:
   `0` success.
   `1` query/usage error.
   `2` I/O/path/permission error.
   `3` parse failure coverage error:
   single-file `.org` input failed to parse, or directory query where at least one candidate `.org` file exists but zero candidates were parsed successfully.
   directory query with zero candidate `.org` files is a query/usage error (`1`) and is not a parse coverage failure.
   `--strict` turns any parse failure in directory mode into exit code `3`.
15. JSON renderer scope:
   `Render_json` is excluded from MVP implementation and architecture.
   MVP implements text renderer only.
16. Regex dependency policy:
   default/release builds use `re2` only.
   `pcre2` (LGPL) is allowed only as an explicit non-default local build profile and is excluded from official release binaries.
17. Query grammar policy for MVP:
   publish a minimal canonical grammar with fixed operator precedence.
   predicate precedence is `comparison` > `and` > `or`.
18. Directory result ordering policy:
   multi-file results are sorted by normalized relative file path, then by `start_line`, then by `end_line`.
   normalized relative path key uses POSIX separator normalization (`\` -> `/`) and Unicode NFC normalization.
   no case folding in MVP; sorting remains case-sensitive.
19. Parse warning format policy:
   parse-failure warnings in directory mode use a fixed single-line template for stable snapshots.
20. Query text matching policy:
   `.section("title")` and `.section_contains("term")` are case-sensitive in MVP.
   `.search("term")` is case-insensitive by default.
   regex-mode `.search("/.../flags")` follows explicit flags (`i` enables case-insensitive matching).
21. Regex pattern-string policy:
   slash-delimited regex pattern-in-string format is canonical: `"/pattern/flags"`.
   supported flags in MVP: `i`, `m`, `s`.
   unsupported flags or malformed pattern strings return query error (exit `1`).
22. Date evaluation determinism policy:
   relative date selectors are evaluated against a reference clock `now` and timezone `tz`.
   defaults are system local `now` and local timezone.
   CLI supports `--now <RFC3339>` and `--tz <IANA_TZ>` overrides for deterministic tests.
   `--now` uses strict RFC3339 date-time with explicit timezone offset (`Z` or `+/-HH:MM`).
   when both `--now` and `--tz` are provided, `--now` is interpreted as a local civil datetime in timezone `tz` for date-window anchoring (`D0`, `W0`), without calendar-date shift via timezone conversion.
   to avoid ambiguity, the explicit offset in `--now` must match the effective offset of `tz` at that local datetime; mismatch is query error (exit `1`).
   accepted examples: `2026-02-17T10:30:00Z`, `2026-02-17T13:30:00+03:00`.
   rejected examples: `2026-02-17`, `2026-02-17 10:30:00`, `2026-02-17T10:30:00` (no offset), and `--now`/`--tz` offset mismatch.
23. Parse warning reason normalization policy:
   parse-failure warnings use stable reason codes.
   canonical code set for MVP: `syntax_error`, `unsupported_construct`, `invalid_utf8`, `internal_parser_error`.
24. Length stage policy:
   canonical collection-size stage is selector `.length`.
   bare `length` stage is not supported in MVP and returns query error (exit `1`).
25. Parser implementation policy for MVP:
   choose **Option A**.
   implement a focused custom Org subset parser (Menhir or Angstrom) as the only MVP parser backend.
   tree-sitter integration is out of MVP scope and may be evaluated post-v1 without changing query semantics.
26. Directory path-sort normalization policy:
   canonical sort key is normalized relative path with separator normalization (`\` -> `/`) + Unicode NFC.
   comparator is lexicographic ascending on normalized UTF-8 text.
   no case folding in MVP.
27. Postfix chaining policy for MVP:
   postfix chaining after indexing/slicing is supported.
   dot-field access on selector results after index/slice is valid (for example `.headings[0].title`).
28. Directory candidate counting policy:
   `candidate_org` is counted after traversal filters are applied.
   MVP traversal filters include hidden-path skip and symlink skip.
   invariant: `parsed_ok + parse_failed = candidate_org`.

## 1. Summary

`oq` is a local CLI for structure-first querying of Org Mode documents (`.org`), inspired by `mq`.  
The goal is to let agents and humans inspect document structure first, then extract only relevant content, reducing token and context waste.

Core principle:

1. Inspect structure (`.tree`)
2. Locate relevant nodes (`.search`, `.headings`, `.todos`)
3. Extract targeted content (`.section(...) | .text`, `.code(...)`)
4. Let the agent/user do final reasoning

## 2. Problem Statement

Current agent workflows often read full files even when only one subtree is needed.  
For large Org notes, knowledge bases, or agenda files, this causes:

1. High token usage
2. Slow navigation to relevant sections
3. Reduced context quality

`oq` should provide deterministic, local, no-index querying over Org structure.

## 3. Goals and Non-Goals

### Goals

1. Query `.org` files and directories from CLI with a jq-like pipeline syntax.
2. Support structure exploration (`.tree`) and scoped extraction (`.section | .text`).
3. Represent Org-specific entities (headings, TODO state, tags, properties, planning, drawers, source blocks, tables, links).
4. Work offline as a single local executable.
5. Keep output deterministic and human/agent readable.

### Non-Goals (MVP)

1. Editing Org files (read-only tool in MVP).
2. Full Org Babel execution.
3. Emacs runtime dependency.
4. LLM or embedding-based ranking/indexing.
5. Cross-format support beyond `.org` (can be Phase 2+).

## 4. Target Users

1. AI coding agents querying project docs, notes, ADRs, and task files in Org.
2. Developers and researchers using Org as personal/work knowledge base.
3. Power users who need scriptable extraction from large Org trees.

## 5. Primary Use Cases

1. "Find all sections about OAuth in `notes/` and extract only relevant subtree text."
2. "List open TODO items tagged `:backend:` with DEADLINE in this week."
3. "Extract all `src` blocks in language `ocaml` under section `Implementation`."
4. "Show directory structure of all Org files before deciding what to open."
5. "Fetch property drawer values (for example `ID`, `OWNER`, `STATUS`) for matching headings."

## 6. User Experience

CLI shape:

```bash
oq <file|directory> [query]
```

No query:

```bash
oq notes.org
```

prints basic file summary (format, heading counts, TODO counts, etc).

Query examples:

```bash
oq notes.org .tree
oq notes.org ".tree('full')"
oq notes.org ".section('Implementation') | .text"
oq notes.org ".section('Implementation', 120:168) | .text"
oq notes.org ".section_contains('Implement')"
oq notes.org ".code('ocaml')"
oq notes.org ".todos | filter(.state == 'TODO')"
oq notes.org ".headings[0]"
oq notes.org ".headings[0].title"
oq notes.org ".headings | .length"
oq notes.org ".search('oauth')"
oq notes.org ".search('/oauth|oidc/i')"
oq tasks.org ".deadline('this_week')"
oq notes/ ".tree('full')"
oq notes/ ".search('deadline')"
```

## 7. Functional Requirements

### 7.1 Input and Traversal

1. Accept a file path or directory path.
2. Traverse only `.org` files in directory mode.
3. Skip hidden files/dirs by default.
4. Skip symlinked files/dirs by default.
5. Gracefully skip parse failures and continue directory operations.
6. Support `--strict` in directory mode: any parse failure returns non-zero exit (`3`).
7. `candidate_org` is defined after hidden/symlink skip filters are applied.
8. If directory mode has zero candidate `.org` files (after traversal filters), return query/usage error (`1`) with actionable message.

### 7.2 Org Parsing and Data Model

Parser must build an indexed document model with:

1. Headings:
   level, raw title, TODO keyword, priority cookie, tags, line start/end.
2. Section hierarchy:
   parent/children and subtree line ranges.
3. Planning metadata:
   `SCHEDULED`, `DEADLINE`, `CLOSED`.
   Parse into normalized date/time values when possible.
4. Property drawers:
   key/value map per heading.
5. Drawers and logs:
   at least preserve as text range for extraction.
6. Blocks:
   source blocks (`#+begin_src`), example blocks, quote blocks.
7. Links:
   bracket links and plain links when parseable.
8. Tables:
   rows/columns.
9. File-level keywords:
   `#+TITLE`, `#+AUTHOR`, `#+FILETAGS`, etc.
10. TODO keyword config:
   parse `#+TODO` sequence and expose open/closed state groups.

### 7.3 Query Language (MVP)

MVP should stay mostly compatible with `mq` mental model:

1. Selectors:
   `.tree`, `.tree("compact"|"preview"|"full")`, `.headings`, `.headings(N)`, `.section("title")`, `.section("title", start:end)`, `.section_contains("term")`, `.sections`, `.code`, `.code("lang")`, `.links`, `.tables`, `.search("term")`, `.text`, `.length`.
2. Org-specific selectors:
   `.todos`, `.done`, `.properties`, `.property("KEY")`, `.scheduled`, `.scheduled("range")`, `.deadline`, `.deadline("range")`, `.closed`, `.closed("range")`, `.tags`.
3. Pipes:
   `<selector> | <selector/function>`.
4. Filtering:
   `filter(predicate)` only.
5. Predicates/operators:
   `==`, `!=`, `<`, `<=`, `>`, `>=`, `and`, `or`.
6. Functions:
   `map(...)`, `contains(...)`, `startswith(...)`, `endswith(...)`.
7. Indexing/slicing:
   `[i]`, `[start:end]` on collections.
8. Relative date ranges:
   support at least `today`, `tomorrow`, `yesterday`, `this_week`, `next_7d`, `overdue`.
9. Date filter semantics:
   ranges are evaluated in timezone `tz` (default: local system timezone).
   evaluation anchor is `now` (default: local system clock).
   `--now <RFC3339>` and `--tz <IANA_TZ>` override defaults for deterministic execution/tests.
   `--now` must be RFC3339 date-time with explicit offset; date-only and offset-less local datetime are invalid.
   when both `--now` and `--tz` are provided, `--now` is interpreted as local civil datetime in `tz` for anchor-date calculations, with no timezone-conversion date shift.
   the explicit offset in `--now` must match timezone `tz` at that local datetime; mismatch is query/usage error (`1`).
   For date-only tokens, boundaries follow Org semantics (`<today>` at 00:00 in timezone `tz`).
10. Org-aligned relative window definitions:
   let `D0` be date start in timezone `tz` (`today 00:00`) and `W0` be start of current week using Org default week start (Monday).
   `today` = `[D0, D0 + 1d)`.
   `tomorrow` = `[D0 + 1d, D0 + 2d)`.
   `yesterday` = `[D0 - 1d, D0)`.
   `this_week` = `[W0, W0 + 7d)` (alias of Org `thisweek` semantics, Monday-start).
   `next_7d` = `[D0, D0 + 7d)` (calendar-day window including today).
11. `overdue` semantics:
   for `.deadline("overdue")` and `.scheduled("overdue")`, match entries where target timestamp is strictly before `D0`.
   entries in DONE state are excluded using file TODO config (`#+TODO`) or fallback `TODO | DONE`.
   entries without a TODO keyword are treated as not done.
12. Emacs-config independence:
   `oq` does not read Emacs runtime variables.
   even though Org allows week-start customization (`org-agenda-start-on-weekday`), MVP pins `this_week` to Monday for deterministic CLI behavior.
13. Text matching defaults:
   `.section("title")` and `.section_contains("term")` are case-sensitive exact/substring matches.
   `.search("term")` is case-insensitive substring search in MVP.
   regex-mode `.search("/.../flags")` uses explicit flags in a string argument; no implicit case folding without `i`.
14. `.section` ambiguity semantics:
   when `.section("title")` has multiple matches, evaluator returns an error and prints candidates as `title (lines start:end)`.
   `.section("title", start:end)` must match both title and exact indexed subtree range, otherwise return "section not found".
15. Canonical ambiguity error template:
   text output for ambiguous section selection is fixed for MVP:
   `Error: ambiguous section title "<title>" (<count> matches)`
   candidate lines are rendered one per line as:
   `  - <title> (lines <start>:<end>)`
   final hint line is rendered as:
   `Hint: use .section("<title>", <start>:<end>)`
   where `<start>:<end>` in hint is one of the candidate ranges.
16. Minimal canonical query grammar (MVP):
   `query := pipe_stage ("|" pipe_stage)*`
   `pipe_stage := atom_stage postfix*`
   `atom_stage := selector_stage | function_stage`
   `selector_stage := "." IDENT ("(" arg_list? ")")?`
   `function_stage := "filter" "(" predicate ")" | "map" "(" expr ")"`
   `postfix := index_suffix | field_suffix`
   `index_suffix := "[" INT "]" | "[" INT? ":" INT? "]"`
   `field_suffix := "." IDENT`
   `predicate := disjunction`
   `disjunction := conjunction ("or" conjunction)*`
   `conjunction := comparison ("and" comparison)*`
   `comparison := expr (("=="|"!="|"<"|"<="|">"|">=") expr)?`
   `expr := path | literal | call | "(" predicate ")"`
   `path := "." IDENT ("." IDENT)*`
   `call := IDENT "(" arg_list? ")"`
   `arg_list := expr ("," expr)*`
   `literal := string | number | boolean`
   `string := DQUOTE chars_dq DQUOTE | SQUOTE chars_sq SQUOTE`
   `chars_dq := (ESC | ANY_BUT_DQUOTE_OR_BACKSLASH)*`
   `chars_sq := (ESC | ANY_BUT_SQUOTE_OR_BACKSLASH)*`
   `ESC := "\\" ("\\" | "/" | "n" | "r" | "t" | "\"" | "'" )`
   allowed regex flags in pattern-strings: `i`, `m`, `s`.
   `number := INT | FLOAT`
   `boolean := "true" | "false"`
   in `.search("...")`, slash-delimited pattern-strings are interpreted as regex with optional flags (`"/pattern/flags"`).
   invalid/unsupported regex flags and malformed regex pattern-strings are query errors (exit `1`).
   postfix chaining is left-associative on the immediately preceding value stage (for example `.headings[0]`, `.headings[0].title`, `.todos[1:3].state`).
   bare indexing stage is invalid (for example `... | [0]`).
   collection size is expressed as selector stage `.length`; bare `length` stage is invalid.

### 7.4 Directory Mode

1. `.tree` variants on directories:
   file list + line counts + top headings (+ previews in full mode).
2. `.search("term")` across all parsed `.org` files.
3. `.search(...)` regex mode:
   if argument is slash-delimited pattern-string (for example `"/oauth|oidc/i"`), treat as regex.
   canonical format is `"/pattern/flags"` with flags from `{i,m,s}` only.
   slash in pattern is escaped as `\/`; unsupported flags are query errors (exit `1`).
4. Results grouped by file with heading/line references.
5. Directory query output must include parse summary counters:
   `candidate_org`, `parsed_ok`, `parse_failed`, `skipped_hidden`, `skipped_symlink`.
   `candidate_org` is post-filter and must satisfy `candidate_org = parsed_ok + parse_failed`.
6. Multi-file result ordering is canonical:
   primary key: normalized relative file path (lexicographic, ascending).
   path normalization for sort key: separator `\` -> `/`, Unicode NFC, case-sensitive.
   secondary key within file: `start_line` ascending, then `end_line` ascending.

### 7.5 Output Formatting

1. Human-readable text output by default.
2. Deterministic ordering (stable traversal, deterministic sort).
3. Errors include actionable hints (unknown selector, missing argument, wrong type).
   Ambiguity errors must include candidate headings with `start:end` ranges and hint to use `.section("title", start:end)`.
   The wording and line layout for ambiguous section errors must follow the canonical template from 7.3.15.
   Parse failures in directory mode are printed as warnings with file path and normalized reason code.
   Canonical parse warning template:
   `Warning: failed to parse <path>: <reason_code>`
   where `<reason_code>` is one of: `syntax_error`, `unsupported_construct`, `invalid_utf8`, `internal_parser_error`.
4. JSON output is out of scope for MVP (text output only).

### 7.6 Exit Codes

1. `0`: command completed successfully.
   In directory mode this requires `candidate_org > 0` and `parsed_ok > 0` (empty match results are still success).
2. `1`: query/usage error.
   Examples: unknown selector, wrong argument type, malformed filter predicate, invalid regex pattern-string, directory mode with zero candidate `.org` files.
3. `2`: I/O error.
   Examples: input path not found, permission denied, unreadable file.
4. `3`: parse coverage failure.
   single-file mode: input `.org` file failed to parse.
   directory mode: returned when all candidate `.org` files failed to parse (candidate count > 0).
   with `--strict`, returned when any candidate `.org` file fails to parse.
5. Time override validation:
   invalid `--now` or invalid `--tz` value is a query/usage error (`1`).
   invalid `--now` includes non-RFC3339 input, RFC3339 without explicit timezone offset, and `--now`/`--tz` offset mismatch at the provided local datetime.

## 8. Non-Functional Requirements

1. Local-only execution; no network required for query path.
2. Startup time under 100ms for small files (target).
3. Directory tree and search should scale to at least 500 Org files in a repo.
4. Predictable memory behavior (no unbounded retention of full intermediate AST snapshots).
5. Cross-platform support: macOS + Linux in MVP, Windows optional.

## 9. Technical Architecture (OCaml)

### 9.1 Stack

1. OCaml 5.x
2. Jane Street:
   `base`, `core`, `stdio`, `ppx_jane`
3. CLI:
   `cmdliner`
4. Parsing:
   custom Org subset parser (Menhir or Angstrom) for MVP.
   tree-sitter integration is post-MVP and non-blocking for v1 deliverables.
5. Regex/text utilities:
   `re2` in default/release build.
   `pcre2` binding is non-default and must stay outside official release artifacts.

### 9.2 Internal Modules (proposed)

1. `Org_parser`:
   parse raw text into AST + token spans.
2. `Org_index`:
   derive heading tree, lookup indexes, and reverse maps.
3. `Query_lexer` / `Query_parser`:
   parse MQL-like syntax.
4. `Query_eval`:
   execute selectors/functions over typed values.
5. `Render_text` (MVP only).
6. `Cli`:
   arg parsing, file/dir dispatch, exit codes.

### 9.3 Data Types (high level)

1. `heading` record:
   `level`, `title`, `todo_state`, `priority`, `tags`, `start_line`, `end_line`.
2. `section` record:
   `heading`, `children`, `content_spans`, `properties`, `planning`.
3. `code_block` record:
   `lang`, `params`, `content`, `start_line`, `end_line`.
4. `query_value` variant:
   scalar, list, object-like, section, heading, code block, link, table.

## 10. Milestones

### M0: Spec and fixtures

1. Finalize selectors and exact semantics.
2. Build Org fixture corpus:
   nested headings, TODO workflows, property drawers, mixed blocks.

### M1: Parser + indexes

1. Parse headings, sections, TODO, tags, planning, properties.
2. Implement `.tree`, `.headings`, `.section`, `.sections`, `.search`, `.text`.

### M2: Query language

1. Pipe execution.
2. `filter`, `map`, predicates, indexing/slicing.
3. Better diagnostics.

### M3: Org enrichments

1. `.todos`, `.deadline`, `.scheduled`, `.property`, including relative date filters.
2. `.code("lang")`, `.links`, `.tables`.

### M4: Hardening

1. Performance pass and profiling.
2. Compatibility test matrix.
3. Packaging + release automation.

## 11. Success Metrics

1. Token reduction benchmark versus naive `cat` workflow on representative Org corpus:
   target >= 60% average reduction.
2. Query latency:
   p95 < 200ms for single-file queries on medium documents.
3. Parse reliability:
   >= 95% of corpus files successfully indexed (with graceful degrade).
4. User productivity:
   reduced number of commands to reach target section (baseline vs `grep + manual open`).

## 12. Risks and Mitigations

1. Org syntax complexity:
   start with explicit MVP subset and publish unsupported constructs.
2. Parser correctness:
   golden tests with line-accurate spans.
3. Query language scope creep:
   freeze grammar for v1 before adding advanced selectors.
4. Dependency risk:
   avoid copyleft parser dependency unless project license explicitly allows it.

## 13. Testing Strategy

1. Unit tests for lexer/parser/evaluator.
2. Golden snapshot tests for `.tree`, `.search`, `.section | .text`.
3. Property tests for parser invariants (parent/child range containment, stable ordering).
4. Integration tests for CLI behavior and exit codes.
   include exit-code matrix tests for directory mode with zero candidates, partial parse failures, all-candidates-failed, and `--strict`.
5. Bench tests on synthetic large Org trees.
6. Golden tests for ambiguity diagnostics:
   verify exact multiline output for `.section("title")` with duplicate headings and for `.section("title", start:end)` not found.
7. Date semantics boundary tests:
   verify `today/tomorrow/yesterday/this_week/next_7d/overdue` around local midnight and Monday week boundary.
   include deterministic runs with fixed `--now` and `--tz`.
8. Time override parsing tests:
   verify accepted/rejected `--now` forms (strict RFC3339 with required offset) and error code `1` on invalid input.
   verify `--now` + `--tz` combined mode: local-civil anchor semantics (no date shift) and rejection on offset mismatch.
9. Negative query diagnostics tests:
   verify stable errors for malformed query syntax, unknown selector, invalid regex pattern-strings, wrong argument types, and out-of-bounds indexing/slicing.
10. Query grammar/precedence tests:
   verify parser behavior for `and/or` precedence and parenthesized predicates.
   verify indexing/slicing postfix binding (`.headings[0]`, `.todos[1:3]`) and chained postfix field access (`.headings[0].title`, `.todos[1:3].state`).
   verify rejection of bare `| [i]` stages.
   verify `.length` acceptance and rejection of bare `| length` stage.
11. Directory ordering and warning-format tests:
   verify stable multi-file sort keys and exact warning template line for parse failures (`reason_code` enum only).
   include path-key normalization checks for separator normalization and Unicode NFC (without case folding).
   verify counter invariants and traversal-filter accounting:
   `candidate_org = parsed_ok + parse_failed`; hidden and symlink skips are excluded from `candidate_org` and counted in `skipped_hidden`/`skipped_symlink`.
12. Regex pattern-string parser tests:
   verify `"/pattern/flags"` parsing inside `.search("...")`, escaped slash handling (`\/`), and rejection of unsupported flags.
13. Match-sensitivity tests:
   verify case-sensitive behavior for `.section`/`.section_contains` and default case-insensitive behavior for `.search("term")`.

## 14. Open Questions for Product Decision

No open product questions. PRD is finalized.

## 15. Reference Inputs Used for This Draft

1. Upstream repository and docs for `mq`:
   https://github.com/muqsitnawaz/mq
2. `mq` syntax reference:
   https://github.com/muqsitnawaz/mq/blob/main/docs/syntax.md
3. Local skill prompt for `mq`:
   `~/.emacs.d/ellama/skills/mq/SKILL.md`
4. Org Manual: Matching tags and properties (relative date tokens and `<today>` boundary at 00:00):
   https://orgmode.org/manual/Matching-tags-and-properties.html
5. Org Manual: Weekly/daily agenda (week-start default and `org-agenda-start-on-weekday`):
   https://orgmode.org/manual/Weekly_002fdaily-agenda.html
6. Org Manual: Deadlines and Scheduling (deadline/scheduled overdue behavior until done):
   https://orgmode.org/manual/Deadlines-and-Scheduling.html
7. Org Manual: The clock table (`today`/`yesterday`/`thisweek` relative block tokens, Monday default):
   https://orgmode.org/manual/The-clock-table.html

## 16. Parser Option Notes (Quick Scan)

1. Tree-sitter Org grammar exists and could be wrapped in a post-MVP track if needed:
   https://github.com/milisims/tree-sitter-org
2. MVP is locked to a focused custom parser for deterministic behavior and license control.

Implication: for v1, parser backend choice is closed; future parser experiments must preserve locked query/output semantics.

## 17. Licensing Tradeoff Snapshot

This snapshot is for engineering planning (not legal advice).

1. Core stack licenses are permissive:
   `base`/`core`/`ppx_jane` are MIT; `cmdliner` is ISC.
2. Regex dependency options:
   `re2` is MIT (permissive) and is the default/release option;
   `pcre2` OCaml binding is LGPL-2.1-or-later with OCaml linking exception and is non-default only.
3. Org parser options:
   custom parser keeps licensing fully under project control;
   `tree-sitter-org` is MIT;
   AGPL parser dependencies increase downstream obligations for redistribution/modification.

Recommended default policy for `oq`:

1. Prefer permissive dependencies only (MIT/ISC/BSD/Apache) for v1.
2. Do not include AGPL/LGPL dependencies in the default distribution.
3. Re-evaluate copyleft dependencies only after explicit product/legal approval.
