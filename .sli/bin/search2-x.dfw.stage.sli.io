#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-horizontal \
    search2-1.dfw.stage.sli.io \
    search2-2.dfw.stage.sli.io \
    search2-3.dfw.stage.sli.io \
