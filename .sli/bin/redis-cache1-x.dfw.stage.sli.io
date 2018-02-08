#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    redis-cache1-1.dfw.stage.sli.io \
    redis-cache1-2.dfw.stage.sli.io \