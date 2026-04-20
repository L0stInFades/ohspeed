#!/usr/bin/env sh
set -eu

REPO="${OHSPEED_REPO:-L0stInFades/ohspeed}"
VERSION="${OHSPEED_VERSION:-latest}"
INSTALL_DIR="${OHSPEED_INSTALL_DIR:-$HOME/.local/bin}"

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"

case "$os" in
  darwin) platform="darwin" ;;
  linux) platform="linux" ;;
  *)
    echo "unsupported operating system: $os" >&2
    exit 1
    ;;
esac

case "$arch" in
  x86_64|amd64) target_arch="amd64" ;;
  arm64|aarch64) target_arch="arm64" ;;
  *)
    echo "unsupported architecture: $arch" >&2
    exit 1
    ;;
esac

asset="ohspeed-${platform}-${target_arch}.tar.gz"

if [ "$VERSION" = "latest" ]; then
  url="https://github.com/${REPO}/releases/latest/download/${asset}"
else
  case "$VERSION" in
    v*) tag="$VERSION" ;;
    *) tag="v$VERSION" ;;
  esac
  url="https://github.com/${REPO}/releases/download/${tag}/${asset}"
fi

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/ohspeed.XXXXXX")"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT INT TERM

mkdir -p "$INSTALL_DIR"
archive="$tmpdir/$asset"

echo "downloading $url"
curl -fsSL "$url" -o "$archive"
tar -xzf "$archive" -C "$tmpdir"
install -m 0755 "$tmpdir/ohspeed" "$INSTALL_DIR/ohspeed"

echo "installed: $INSTALL_DIR/ohspeed"
case ":$PATH:" in
  *":$INSTALL_DIR:"*) ;;
  *)
    echo "add $INSTALL_DIR to PATH if it is not already there"
    ;;
esac
