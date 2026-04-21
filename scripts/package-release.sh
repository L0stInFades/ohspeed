#!/usr/bin/env bash
set -euo pipefail

output_dir="${1:-dist}"
platform="${2:-}"
arch="${3:-}"

if [[ -z "$platform" ]]; then
  case "$(uname -s)" in
    Darwin) platform="darwin" ;;
    Linux) platform="linux" ;;
    *)
      echo "unsupported operating system" >&2
      exit 1
      ;;
  esac
fi

if [[ -z "$arch" ]]; then
  case "$(uname -m)" in
    x86_64|amd64) arch="amd64" ;;
    arm64|aarch64) arch="arm64" ;;
    *)
      echo "unsupported architecture" >&2
      exit 1
      ;;
  esac
fi

dune build @install

asset="ohspeed-${platform}-${arch}"
stage="$(mktemp -d "${TMPDIR:-/tmp}/ohspeed-package.XXXXXX")"
cleanup() {
  rm -rf "$stage"
}
trap cleanup EXIT INT TERM

mkdir -p "$output_dir"
cp "_build/install/default/bin/ohspeed" "$stage/ohspeed"
cp README.md LICENSE "$stage/"

archive="$output_dir/${asset}.tar.gz"
checksum="$archive.sha256"
checksum_name="$(basename "$archive")"

tar -C "$stage" -czf "$archive" ohspeed README.md LICENSE
if command -v shasum >/dev/null 2>&1; then
  shasum -a 256 "$archive" | sed "s#  .*#  $checksum_name#" > "$checksum"
elif command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$archive" | sed "s#  .*#  $checksum_name#" > "$checksum"
else
  echo "missing checksum tool: need shasum or sha256sum" >&2
  exit 1
fi
