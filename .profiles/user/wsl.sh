#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

if [[ $UID == 0 ]]; then
  echo "don't run as root" 1>&2
  exit 1
fi

setup_shfmt() {
  local tmp=
  local version=3.1.2
  local sha256=c5794c1ac081f0028d60317454fe388068ab5af7740a83e393515170a7157dce
  [[ $(shfmt --version 2>&1) == "v$version" ]] && return

  tmp=$(mktemp)
  trap "{ rm -f $tmp; }" EXIT
  curl -L -o "$tmp" "https://github.com/mvdan/sh/releases/download/v${version}/shfmt_v${version}_linux_amd64"
  sha256sum -c <<<"$sha256 $tmp"
  chmod +x "$tmp"
  mv "$tmp" ~/.local/bin/shfmt
}

setup_shell() {
  local shell=/usr/bin/zsh
  if ! getent passwd "$USER" | grep "$shell"; then
    chsh -s /usr/bin/zsh
  fi
}

setup_shfmt
setup_shell
