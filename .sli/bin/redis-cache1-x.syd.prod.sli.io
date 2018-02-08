#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    redis-cache1-1.syd.prod.sli.io \
    redis-cache1-2.syd.prod.sli.io \
