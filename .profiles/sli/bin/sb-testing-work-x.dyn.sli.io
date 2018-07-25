#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    sb-testing-worker-1.dyn.sli.io \
    sb-testing-worker-2.dyn.sli.io \
    sb-testing-worker-3.dyn.sli.io \
