# oq Agent Playbook (M4)

This playbook defines practical query patterns for iterative agent loops.
Use it with the stable contract in `docs/agent-contract.md`.

## 1. Deterministic Defaults

Use these defaults for reproducible runs:

1. Pin time-dependent queries with both `--now` and `--tz`.
2. Keep query strings exact across retries.
3. Prefer structure-first selectors before `.text`.
4. Use `--strict` only when parse completeness is mandatory.

Example pinned run:

```bash
oq --now 2026-02-17T08:00:00-08:00 --tz America/Los_Angeles notes.org ".scheduled('next_7d') | map(.title)"
```

## 2. High-Signal Query Cookbook

### 2.1 Section Discovery and Extraction

```bash
oq notes.org ".headings | map(.title)"
oq notes.org ".headings | filter(startswith(.title, 'In')) | map(.title)"
oq notes.org ".section('Inbox', 42:68) | .text"
oq notes.org ".section_contains('release') | .length"
```

### 2.2 Content Search

```bash
oq notes.org ".search('oauth')"
oq notes.org ".search('/oauth|oidc/i')"
oq notes.org ".search('/^\\*\\* TODO/m')"
```

### 2.3 TODO and Planning Views

```bash
oq tasks.org ".todos | map(.title)"
oq tasks.org ".done | map(.title)"
oq tasks.org ".scheduled('today') | map(.title)"
oq tasks.org ".deadline('overdue') | map(.title)"
oq tasks.org ".closed('this_week') | map(.title)"
```

### 2.4 Org Metadata

```bash
oq notes.org ".properties | map(.key)"
oq notes.org ".property('OWNER') | map(.value)"
oq notes.org ".tags"
oq notes.org ".links | map(.target)"
oq notes.org ".code('ocaml') | .length"
oq notes.org ".tables | .length"
```

### 2.5 Directory Triage

```bash
oq notes/ ".headings | .length"
oq notes/ ".search('incident')"
oq notes/ ".todos | filter(.state == 'NEXT') | map(.title)"
oq --strict notes/ ".headings[0].title"
```

## 3. Canonical Agent Loop

Use a stable 4-step loop:

1. Discover: `.headings`, `.todos`, `.search(...)`.
2. Narrow: `filter(...)`, indexing/slicing, `.section(..., start:end)`.
3. Extract: `.text` only after scope is small.
4. Validate: rerun the same command and compare output bytes.

## 4. Troubleshooting Flow

| Signal | Meaning | Next Action |
| --- | --- | --- |
| `exit 1` with `unknown selector` | Query typo or unsupported selector | Fix selector spelling/shape and retry once |
| `exit 1` with `unknown field ... for heading` | Wrong field on current type | Switch field (`.title`, `.state`, `.tags`, `.start_line`, `.end_line`) |
| `exit 1` with `ambiguous section title` | Duplicate heading titles | Retry with `.section(\"title\", start:end)` from hint |
| `exit 1` with `no candidate .org files found...` | Directory had no candidate Org files | Verify path and hidden/symlink filters |
| `exit 3` with `parse coverage failure...` | Candidate files exist but none parsed | Narrow scope or fix malformed files |
| `exit 3` with `strict mode failed...` | At least one parse failure under strict mode | Disable `--strict` for best-effort pass or fix broken file |
| `exit 2` | I/O/path/permission failure | Check path existence and permissions |

## 5. Deterministic Run Checklist

1. Always include `--now` and `--tz` for relative date windows.
2. Keep directory queries in stable path roots; avoid moving files between retries.
3. Treat warning lines as part of deterministic output checks.
4. In automation loops, retry at most once for `exit 1` after deterministic correction.
5. Use exact query snapshots in tests for regression detection.

For repository triage/release/CI command standards, see `docs/gh-operations.md`.
