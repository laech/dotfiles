#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    redis1-1.dfw.stage.sli.io \
    redis1-2.dfw.stage.sli.io \
