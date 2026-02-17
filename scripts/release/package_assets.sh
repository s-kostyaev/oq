#!/usr/bin/env bash
set -euo pipefail

tag="${1:?usage: package_assets.sh <tag> [binary_path] [dist_dir]}"
binary_path="${2:-_build/install/default/bin/oq}"
dist_dir="${3:-dist}"

test -x "$binary_path"

platform="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"
case "$arch" in
  x86_64|amd64) arch="x86_64" ;;
  arm64|aarch64) arch="arm64" ;;
esac

package_root="oq-${tag}-${platform}-${arch}"
archive_name="${package_root}.tar.gz"
payload_dir="${dist_dir}/${package_root}"
archive="${dist_dir}/${archive_name}"
checksum="${archive}.sha256"

rm -rf "$payload_dir" "$archive" "$checksum"
mkdir -p "$payload_dir"

cp "$binary_path" "${payload_dir}/oq"
cp README.md LICENSE "$payload_dir/"

tar -czf "$archive" -C "$dist_dir" "$package_root"
(
  cd "$dist_dir"
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$archive_name" > "${archive_name}.sha256"
  else
    shasum -a 256 "$archive_name" > "${archive_name}.sha256"
  fi
)
