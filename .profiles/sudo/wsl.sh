#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

if [[ $UID == 0 ]]; then
  echo "don't run as root" 1>&2
  exit 1
fi

readonly packages=(

  # For building git-credential-libsecret
  libglib2.0-dev
  libsecret-1-dev

  build-essential
  curl
  tmux
  zsh
  vim
  emacs-nox
  shellcheck
  source-highlight
  maven
  openjdk-8-jdk
  openjdk-8-source
  openjdk-11-jdk
  openjdk-11-source
)

sudo apt install -y "${packages[@]}"

if [[ ! -f /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret ]]; then
  sudo make --directory=/usr/share/doc/git/contrib/credential/libsecret
fi
