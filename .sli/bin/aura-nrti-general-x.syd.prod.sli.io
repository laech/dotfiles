#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    aura-nrti-general-1.syd.prod.sli.io \
    aura-nrti-general-2.syd.prod.sli.io \
    aura-nrti-general-3.syd.prod.sli.io \
