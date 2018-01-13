#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    dory2-1.syd.prod.sli.io \
    dory2-2.syd.prod.sli.io \
    dory2-3.syd.prod.sli.io \
    dory2-4.syd.prod.sli.io \
