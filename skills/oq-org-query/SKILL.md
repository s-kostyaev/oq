---
name: oq-org-query
description: Query Org-mode files and directories with oq using deterministic, structure-first, low-token workflows. Use when an agent needs to inspect .org content, locate sections/tasks/metadata, extract only scoped text, or recover from oq query and parse errors.
---

# Query Org Files with `oq`

Use this skill to minimize context usage while keeping results deterministic.

## Enforce Low-Context Strategy

1. Start with structure, not prose.
2. Narrow scope before extracting `.text`.
3. Project only needed fields with `map(...)`.
4. Use directory mode only when cross-file coverage is required.
5. Keep commands stable across retries.

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

## Query Patterns

### Fast document triage

```bash
oq notes.org ".headings | map(.title)"
oq notes.org ".tree"
oq notes.org ".todos | map(.title)"
oq notes.org ".search('incident')"
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
oq notes/ ".todos | filter(.state == 'NEXT') | map(.title)"
oq notes/ ".search('oauth')"
```

Use `--strict` only when parse completeness is required:

```bash
oq --strict notes/ ".headings | .length"
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
