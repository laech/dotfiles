#!/usr/bin/env bash

set -o nounset
set -o errexit
set -o pipefail

if [[ -n "$WSLENV" ]]; then
  /mnt/c/Program\ Files/Git/mingw64/libexec/git-core/git-credential-manager.exe "$@"
elif [[ -e /usr/libexec/git-core/git-credential-libsecret ]]; then
  /usr/libexec/git-core/git-credential-libsecret "$@"
elif [[ -e /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret ]]; then
  /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret "$@"
else
  echo "No git credential helper found." 1>&2
  exit 1
fi
