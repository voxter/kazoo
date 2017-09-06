#!/bin/bash

VERSION=`git describe --abbrev=0 --tags`
ITTERATION=2
ARCH=x86_64

fpm -s dir \
    -t rpm \
    -n "kazoo" \
    -v $VERSION \
    --iteration $ITTERATION \
    -a $ARCH \
    --prefix /opt/kazoo \
    -d "kazoo-configs-core >= 4.1.16-1" \
    -d "kazoo-sounds" \
    -d "bash-completion" \
    -d "esl-erlang >= 19.3" \
    --after-install scripts/rpm/after-install.sh \
    $@ .