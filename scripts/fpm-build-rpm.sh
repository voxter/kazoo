#!/bin/bash

VERSION=`git describe --tags`
ITTERATION=1
ARCH=x86_64

fpm -s dir \
    -t rpm \
    -n "kazoo" \
    -v $VERSION \
    --iteration $ITTERATION \
    -a $ARCH \
    --prefix /opt/kazoo \
    -d "kazoo-configs-core >= 4.2.3-1" \
    -d "kazoo-sounds" \
    -d "bash-completion" \
    -d "esl-erlang >= 19.3" \
    --after-install scripts/rpm/after-install.sh \
    $@ _rel/kazoo
