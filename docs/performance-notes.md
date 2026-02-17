# Performance Notes (M4)

This note captures the Stage 8 hot-path work for:

1. `.search`
2. directory traversal
3. text rendering paths

## Profiling Method

Use the benchmark gate executable to measure repeated directory `.search` runs:

```bash
dune exec bench/benchmark_check.exe
```

It creates a 500-file synthetic Org corpus and reports:

1. first-run wall clock seconds
2. second-run wall clock seconds

## Applied Optimizations

1. `.search` plain-string mode now compiles a case-insensitive escaped regex once per query stage, avoiding per-heading lowercase-copy allocations.
2. Runtime now builds a heading-id lookup table once per document, removing repeated linear heading lookups in field resolution paths.
3. `.sections` now maps directly from indexed headings instead of resolving heading ids for each section entry.
4. `Runtime.text_for_span` now builds text via a single buffer loop, avoiding `Array.slice` + list allocation chains.
5. Directory scan counters now use direct mutable integer refs to avoid per-increment record allocations.
6. Directory iteration now sorts and iterates arrays directly, avoiding array-to-list conversion on every scanned directory.

## Reproducibility

For stable benchmark comparisons, run on an idle machine and compare multiple runs rather than single samples.
