#!/bin/bash

set -x
set -e

echo "--- :construction: Running make"
make

echo "--- :erlang: Building release"
make build-release

echo "--- :shell: Including bash completion"
cp sup.bash _rel/kazoo/

echo "--- :package: Packaging RPM"
./scripts/fpm-build-rpm.sh
