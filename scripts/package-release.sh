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

tar -C "$stage" -czf "$output_dir/${asset}.tar.gz" ohspeed README.md LICENSE
if command -v shasum >/dev/null 2>&1; then
  shasum -a 256 "$output_dir/${asset}.tar.gz" > "$output_dir/${asset}.tar.gz.sha256"
elif command -v sha256sum >/dev/null 2>&1; then
  sha256sum "$output_dir/${asset}.tar.gz" > "$output_dir/${asset}.tar.gz.sha256"
else
  echo "missing checksum tool: need shasum or sha256sum" >&2
  exit 1
fi
