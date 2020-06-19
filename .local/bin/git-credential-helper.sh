#!/usr/bin/env bash

if [[ ! -z "$WSLENV" ]]; then
  /mnt/c/Program\ Files/Git/mingw64/libexec/git-core/git-credential-manager.exe $@
else
  /usr/lib/git-core/git-credential-libsecret $@
fi

