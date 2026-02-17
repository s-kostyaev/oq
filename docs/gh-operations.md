# GitHub Operations via `gh` CLI (M4)

This document standardizes repository operations that should be performed with the GitHub CLI.

## 1. Prerequisites

1. Authenticate once:

```bash
gh auth login
gh auth status
```

2. Ensure the current repository has `origin` set to GitHub:

```bash
git remote -v
```

## 2. Issue and PR Triage

### 2.1 List active work

```bash
gh issue list --state open --limit 50
gh pr list --state open --limit 50
```

### 2.2 Inspect details

```bash
gh issue view <issue-number>
gh pr view <pr-number>
```

### 2.3 Standard triage actions

```bash
gh issue edit <issue-number> --add-label bug
gh issue edit <issue-number> --add-assignee @me
gh issue comment <issue-number> --body "Reproduced locally; queued for fix."

gh pr review <pr-number> --comment --body "Please add regression coverage for directory mode."
gh pr comment <pr-number> --body "CI is green; ready for merge after one follow-up."
gh pr merge <pr-number> --squash --delete-branch
```

## 3. CI Status Checks

Gate definitions and command mapping live in `docs/ci-gates.md`.

### 3.1 Repository-wide status

```bash
gh run list --limit 20
```

### 3.2 Inspect a failing run

```bash
gh run view <run-id>
gh run view <run-id> --log-failed
```

### 3.3 PR-specific checks

```bash
gh pr checks <pr-number>
```

## 4. Release Operations

For ordered release policy and publish checklist (GitHub binaries -> opam -> Homebrew), see `docs/release-flow.md`.

### 4.1 Create release from a tag

```bash
gh release create v1.0.0 \
  --title "oq v1.0.0" \
  --notes-file docs/release-notes/v1.0.0.md
```

### 4.2 Upload build artifacts

```bash
gh release upload v1.0.0 dist/*
```

### 4.3 Verify release metadata

```bash
gh release view v1.0.0
gh release list --limit 10
```

## 5. Agent Runbook

Use this order for deterministic automation:

1. `gh run list` to verify latest CI status.
2. `gh pr checks <pr-number>` before merge/release actions.
3. `gh release create ...` only after checks are green.
4. `gh release view <tag>` to confirm publish success.
