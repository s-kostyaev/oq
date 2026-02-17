# oq Agent Contract (M0)

This document defines stable CLI behavior that agents can rely on from the bootstrap stage.

## 1. Stable Invocation Shape

```bash
oq <file-or-directory> [query]
```

The query language and selectors are introduced incrementally in later milestones.
Until then, agents should treat any unsupported query as a query/usage failure (`exit 1`).

## 2. Exit Code Contract

1. `0` success.
2. `1` query/usage error.
3. `2` I/O/path/permission error.
4. `3` parse coverage failure.

Agents should route retries based on these classes instead of matching free-form text.

## 3. Canonical Diagnostics

`Error:` prefix:

```text
Error: <message>
```

`Warning:` prefix:

```text
Warning: <message>
```

Canonical parse warning shape:

```text
Warning: failed to parse <path>: <reason_code>
```

`<reason_code>` is one of:
`syntax_error`, `unsupported_construct`, `invalid_utf8`, `internal_parser_error`.

## 4. Deterministic Ordering Rule

Multi-file result references are sorted by:

1. normalized relative path (replace `\` with `/`),
2. `start_line` ascending,
3. `end_line` ascending.

Agents can safely diff outputs between runs using this order.

## 5. Retry Guidance for Agents

1. Exit `0`: proceed to the next planned query step.
2. Exit `1`: rewrite query or arguments, then retry once with corrected syntax.
3. Exit `2`: verify path/permissions and retry after environment correction.
4. Exit `3`: narrow scope, disable strict behavior if appropriate, or skip failed inputs and continue workflow.

## 6. Minimal Agent Playbook (Bootstrap)

1. Run structural query first (when available), then narrow.
2. Avoid full-file extraction until a section is identified.
3. If a query fails with `1`, prefer automatic correction and single retry.
4. Treat output ordering as stable and suitable for exact snapshot assertions.
