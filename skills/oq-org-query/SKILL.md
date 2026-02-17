---
name: oq-org-query
description: Query Org-mode files and directories with oq using deterministic, structure-first, low-token workflows. Use when an agent needs to inspect .org content, locate sections/tasks/metadata, extract only scoped text.
---

# Query Org Files with `oq`

Use this skill to minimize context usage while keeping results deterministic.

`oq` does not decide the answer for you. It externalizes Org structure into context so you can reason from evidence.

```text
Org files -> oq query -> structure in context -> reasoning -> answer
```

## The Pattern

```text
1. Discover  -> oq <path> .tree / .headings / .todos / .search('x')
2. Narrow    -> filter(...), [i], [start:end], .section("title", start:end)
3. Extract   -> .text only after one target section is identified
4. Verify    -> rerun exact command when deterministic output matters
```

## Quick Reference

```bash
# Structure and triage
oq file.org .tree
oq file.org ".headings | map(.title)"
oq dir/ ".tree | .length"
oq dir/ ".search('incident')"

# Tasks and dates
oq tasks.org ".todos | map(.title)"
oq tasks.org ".todos | filter(.state == 'NEXT') | map(.title)"
oq --now 2026-02-17T08:00:00-08:00 --tz America/Los_Angeles tasks.org ".deadline('next_7d') | map(.title)"

# Targeted extraction
oq file.org ".section('Inbox', 42:68) | .text"
oq file.org ".section_contains('release') | .length"

# Metadata
oq file.org ".property('OWNER') | map(.value)"
oq file.org ".tags"
oq file.org ".links | map(.target)"
```

## Enforce Low-Context Strategy

1. Start with structure, not prose.
2. Narrow scope before extracting `.text`.
3. Project only needed fields with `map(...)`.
4. Use directory mode only when cross-file coverage is required.
5. Keep commands stable across retries.
6. Ask for counts first when match sets may be large.

Preferred progression:

1. Discover: `.tree`, `.headings`, `.todos`, `.search(...)`, `.sections | .length`
2. Narrow: `filter(...)`, `[i]`, `[start:end]`, `.section("title", start:end)`
3. Extract: `.text` only after a single target section is identified
4. Verify: rerun exact command when deterministic output matters

## Determinism Rules

For relative date windows, always pin both flags:

```bash
oq --now 2026-02-17T08:00:00-08:00 --tz America/Los_Angeles tasks.org ".deadline('next_7d') | map(.title)"
```

Do not change query text between retries unless correcting a specific error.

## Bounded Output Rules

1. For potentially large results, run `| .length` first.
2. Sample with `[0:10]` or stronger filters before requesting text.
3. Return projected fields (`title`, `state`, `path`, `deadline`) before full section data.
4. Extract `.text` only for the final, smallest possible target.

## Query Patterns

### Fast document triage

```bash
oq notes.org ".headings | map(.title)"
oq notes.org ".tree"
oq notes.org ".tree | .length"
oq notes.org ".tree | filter(.level <= 2) | map(.title)"
oq notes.org ".tree[0:10] | map(.title)"
oq notes.org ".todos | map(.title)"
oq notes.org ".search('incident')"
```

### Tree-first narrowing

```bash
oq notes.org ".tree | filter(startswith(.title, 'In')) | map(.start_line)"
oq notes.org ".tree | filter(.level == 1) | map(.title)"
oq notes/ ".tree | filter(.path == 'roadmap.org') | map(.title)"
oq notes.org ".search('release') | .length"
oq notes.org ".search('release')[0:5]"
```

### Targeted section extraction

```bash
oq notes.org ".section('Inbox', 42:68) | .text"
oq notes.org ".section_contains('release') | .length"
```

### Metadata-first retrieval

```bash
oq notes.org ".property('OWNER') | map(.value)"
oq notes.org ".tags"
oq notes.org ".links | map(.target)"
```

### Directory scans with bounded output

```bash
oq notes/ ".search('oauth') | .length"
oq notes/ ".search('oauth')[0:10]"
oq notes/ ".todos | filter(.state == 'NEXT') | map(.title)"
```

Use `--strict` only when parse completeness is required:

```bash
oq --strict notes/ ".headings | .length"
```

## Anti-Patterns

Bad: immediate broad text extraction

```bash
oq notes/ ".search('incident') | .text"
```

Good: count -> narrow -> extract

```bash
oq notes/ ".search('incident') | .length"
oq notes/ ".search('incident')[0:5] | map(.path)"
oq notes.org ".section('Incident Review', 120:176) | .text"
```

Bad: re-querying structure with no new need

```bash
oq notes/ .tree
oq notes/ .tree
```

Good: reuse what is already in context

```bash
oq notes/ .tree
# Use known paths/titles from the prior map
oq notes.org ".section('Inbox', 42:68) | .text"
```

## Context as Working Memory

Treat previous `oq` output as an index already loaded in context.
Avoid rerunning discovery commands unless files changed or coverage scope changed.
Spend queries on narrowing and extraction, not on rebuilding the same map.

## Examples by Task

Find NEXT tasks this week:

```bash
oq tasks.org ".todos | filter(.state == 'NEXT') | map(.title)"
oq --now 2026-02-17T08:00:00-08:00 --tz America/Los_Angeles tasks.org ".deadline('next_7d') | map(.title)"
```

Find owner of a release section:

```bash
oq notes.org ".section_contains('release')[0:1] | map(.start_line)"
oq notes.org ".property('OWNER') | map(.value)"
```

Locate references to OAuth across files:

```bash
oq notes/ ".search('oauth') | .length"
oq notes/ ".search('oauth')[0:10] | map(.path)"
```

## Error Recovery

1. `exit 1` unknown selector/field:
   correct selector or field and retry once.
2. `exit 1` ambiguous section title:
   rerun with `.section("title", start:end)` using hinted range.
3. `exit 1` query syntax error:
   fix expression shape, especially pipeline postfix placement.
4. `exit 2` I/O/path error:
   verify path and permissions.
5. `exit 3` parse coverage/strict failure:
   remove `--strict` for best effort, or narrow/fix malformed files.

## Output Discipline for Agents

1. Prefer title/state/date/path fields over full section dumps.
2. Extract `.text` only for the final, smallest possible target.
3. Stop after sufficient evidence for the user task; avoid exploratory over-fetch.
4. When many matches exist, return counts first (`.length`), then sample or filter.
