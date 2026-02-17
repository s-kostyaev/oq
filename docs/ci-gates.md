# CI Gates (M4)

This repository enforces CI gates in `.github/workflows/ci.yml`.

## Gate Categories

1. Lint
   `opam lint oq.opam`
2. Unit
   `smoke`, `contract_smoke`, `fixture_corpus`, `org_parser_indexing`, `query_parser`
3. Golden
   `diagnostic_golden`, `query_evaluator_core`
4. Integration
   `query_evaluator_org_enrichments`, `directory_mode`, `agent_workflow_scenarios`
5. Benchmark
   `bench/benchmark_check.exe` (500-file directory search stability and latency sanity)
6. Release Preflight
   release-profile install build plus packaged-asset validation on `ubuntu-latest` and `macos-14`

## Local Reproduction

Run the same sequence locally:

```bash
dune build
opam lint oq.opam
dune exec test/smoke.exe
dune exec test/contract_smoke.exe
dune exec test/fixture_corpus.exe
dune exec test/org_parser_indexing.exe
dune exec test/query_parser.exe
dune exec test/diagnostic_golden.exe
dune exec test/query_evaluator_core.exe
dune exec test/query_evaluator_org_enrichments.exe
dune exec test/directory_mode.exe
dune exec test/agent_workflow_scenarios.exe
dune exec bench/benchmark_check.exe
opam exec -- dune build --profile release @install
bash scripts/release/package_assets.sh preflight-local
bash scripts/release/verify_packaged_assets.sh preflight-local
```
