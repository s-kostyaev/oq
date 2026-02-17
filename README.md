# oq

`oq` is an agent-first CLI for querying Org files with deterministic output.

## Features

- Query selectors for Org structure and content (`.headings`, `.section`, `.search`, `.text`, `.todos`, `.scheduled`, etc.).
- Function pipelines with filtering and projection (`filter(...)`, `map(...)`).
- Deterministic date behavior via `--now` (RFC3339) and `--tz` (IANA timezone).
- Directory mode with stable ordering, parse warnings, and strict parse coverage mode.

## Requirements

- OCaml `>= 5.1`
- dune `>= 3.14`

## Install Locally

```bash
git clone https://github.com/s-kostyaev/oq.git
cd oq
dune build
dune install
```

## Build and Test

```bash
dune build
dune runtest
```

## Usage

```bash
oq [--now RFC3339] [--tz IANA_TZ] [--strict] FILE_OR_DIR [QUERY]
```

Examples:

```bash
oq notes.org ".headings | map(.title)"
oq notes.org ".section('Inbox', 6:8) | .text"
oq notes.org ".todos | filter(.state == 'NEXT') | map(.title)"
oq --now 2026-02-17T08:00:00-08:00 --tz America/Los_Angeles notes.org ".scheduled('next_7d') | map(.title)"
oq --strict notes/ ".headings | .length"
```

## Exit Codes

- `0`: success
- `1`: query or usage error
- `2`: I/O, path, or permission error
- `3`: parse coverage failure

## Documentation

- Product requirements: `docs/PRD-oq-v1.md`
- Agent contract: `docs/agent-contract.md`
- Agent query cookbook: `docs/agent-playbook.md`
- Agent skill (low-context querying): `skills/oq-org-query/SKILL.md`
- CI policy: `docs/ci-gates.md`
- Performance notes: `docs/performance-notes.md`
- GitHub workflow guidance: `docs/gh-operations.md`
- Release pipeline: `docs/release-flow.md`

## License

MIT. See `LICENSE`.
