#!/bin/bash

set -x
set -e

# Required to regenerate .app files in ebin directories
echo "--- :broom: Cleaning build"
make clean

echo "--- :construction: Running make"
make

echo "--- :erlang: Building release"
make build-release

echo "--- :shell: Including bash completion"
make sup_completion
cp sup.bash _rel/kazoo/

echo "--- :package: Packaging RPM"
./scripts/fpm-build-rpm.sh
