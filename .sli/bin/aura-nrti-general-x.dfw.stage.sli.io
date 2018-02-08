#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    aura-nrti-general-1.dfw.stage.sli.io \
    aura-nrti-general-2.dfw.stage.sli.io \
    aura-nrti-general-3.dfw.stage.sli.io \
