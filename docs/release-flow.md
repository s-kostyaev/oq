# Release Flow (M4)

This runbook defines the required release order:

1. GitHub release binaries
2. opam package publication
3. Homebrew formula update

## 1. GitHub Release Binaries

### 1.1 Prepare

1. Ensure CI is green for `main`.
2. Ensure `dune build` and `dune runtest` pass locally.
3. Update versioned notes in `docs/release-notes/<tag>.md` when needed.

### 1.2 Trigger

Push a semver tag (`v*`) to trigger `.github/workflows/release.yml`:

```bash
git tag v1.0.0
git push origin v1.0.0
```

The workflow builds release artifacts for:

1. `ubuntu-latest` (`linux-x86_64`)
2. `macos-13` (`darwin-x86_64`)
3. `macos-14` (`darwin-arm64`)

Each asset is uploaded as:

1. `oq-<tag>-<platform>-<arch>.tar.gz`
2. `oq-<tag>-<platform>-<arch>.tar.gz.sha256`

### 1.3 Verify

```bash
gh release view v1.0.0
gh release download v1.0.0 --pattern "oq-v1.0.0-linux-x86_64.tar.gz*"
```

## 2. Publish to opam

After GitHub assets are published and verified:

1. Update `oq.opam` versioned metadata for the release.
2. Submit/update an `opam-repository` PR referencing the GitHub archive and checksum.
3. Wait for opam-repository CI/merge.

Reference command:

```bash
opam lint oq.opam
```

## 3. Publish to Homebrew

After opam publication is accepted:

1. Update Homebrew formula `url` and `sha256` to the GitHub release tarball.
2. Open/update Homebrew tap PR.
3. Validate install from the updated formula.

Smoke check after publish:

```bash
oq --help
```

## 4. Rollback Rule

If a release issue is detected:

1. Stop opam/Homebrew publication immediately.
2. Fix forward with a new tag (`vX.Y.Z+1`) and regenerated GitHub assets.
3. Do not reuse or overwrite compromised artifacts.
