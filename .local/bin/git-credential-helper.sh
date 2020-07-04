#!/usr/bin/env bash

if [[ -n "$WSLENV" ]]; then
  /mnt/c/Program\ Files/Git/mingw64/libexec/git-core/git-credential-manager.exe "$@"
else
  /usr/share/doc/git/contrib/credential/libsecret/git-credential-libsecret "$@"
fi
