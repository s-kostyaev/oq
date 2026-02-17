# AGENTS.md

## Mandatory Stage Completion Flow

For every implemented stage from `implementation_plan.md`, complete these steps in order:

1. Run checks: `dune build`.
2. Run tests: `dune runtest`.
3. Create a commit with a clear message describing the stage progress.
4. Push the commit to the repository (`origin`, current working branch).

Do not treat a stage as complete until all four steps succeed.
