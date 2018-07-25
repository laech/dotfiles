#!/usr/bin/env bash

"$(dirname $0)/ssh-sli" \
    -u root \
    -n "$(basename $0)" \
    -l even-vertical \
    sb1-1.iad.prod.sli.io \
    sb1-2.iad.prod.sli.io \
    sb1-3.iad.prod.sli.io \
    sb1-4.iad.prod.sli.io \
    sb1-5.iad.prod.sli.io \
    sb1-6.iad.prod.sli.io \
    sb1-7.iad.prod.sli.io \
