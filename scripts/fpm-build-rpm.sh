#!/bin/bash

VERSION=`git describe --abbrev=0 --tags`
ITTERATION=4
ARCH=x86_64

fpm -s dir \
    -t rpm \
    -n "kazoo" \
    -v $VERSION \
    --iteration $ITTERATION \
    -a $ARCH \
    --prefix /opt/kazoo \
    -d "kazoo-configs >= 4.1-5" \
    -d "kazoo-sounds" \
    -d "bash-completion" \
    -d "esl-erlang >= 19.3" \
    --after-install scripts/rpm/after-install.sh \
    $@ .