#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    dory2-5.syd.prod.sli.io \
    dory2-6.syd.prod.sli.io \
    dory2-7.syd.prod.sli.io \
    dory2-8.syd.prod.sli.io \
