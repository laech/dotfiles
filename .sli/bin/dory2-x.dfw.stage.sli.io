#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    dory2-1.dfw.stage.sli.io \
    dory2-2.dfw.stage.sli.io \
    dory2-3.dfw.stage.sli.io \
    dory2-4.dfw.stage.sli.io \
