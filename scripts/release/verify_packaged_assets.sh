#!/usr/bin/env bash
set -euo pipefail

tag="${1:?usage: verify_packaged_assets.sh <tag> [dist_dir]}"
dist_dir="${2:-dist}"

platform="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"
case "$arch" in
  x86_64|amd64) arch="x86_64" ;;
  arm64|aarch64) arch="arm64" ;;
esac

archive="${dist_dir}/oq-${tag}-${platform}-${arch}.tar.gz"
checksum="${archive}.sha256"

if [[ ! -f "$archive" ]]; then
  echo "missing expected archive: $archive" >&2
  exit 1
fi

if [[ ! -f "$checksum" ]]; then
  echo "missing expected checksum: $checksum" >&2
  exit 1
fi

archives=()
while IFS= read -r archive_file; do
  archives+=("$archive_file")
done < <(find "$dist_dir" -maxdepth 1 -type f -name "oq-${tag}-*.tar.gz" | sort)
if [[ ${#archives[@]} -ne 1 || "${archives[0]}" != "$archive" ]]; then
  echo "unexpected archive set for tag $tag in $dist_dir" >&2
  printf '%s\n' "${archives[@]}" >&2
  exit 1
fi

checksums=()
while IFS= read -r checksum_file; do
  checksums+=("$checksum_file")
done < <(find "$dist_dir" -maxdepth 1 -type f -name "oq-${tag}-*.tar.gz.sha256" | sort)
if [[ ${#checksums[@]} -ne 1 || "${checksums[0]}" != "$checksum" ]]; then
  echo "unexpected checksum set for tag $tag in $dist_dir" >&2
  printf '%s\n' "${checksums[@]}" >&2
  exit 1
fi

(
  cd "$dist_dir"
  checksum_name="$(basename "$checksum")"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum -c "$checksum_name"
  else
    shasum -a 256 -c "$checksum_name"
  fi
)
