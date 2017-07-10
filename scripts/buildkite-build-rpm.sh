#!/bin/bash

set -x
set -e

echo "--- :dash: Running make clean"
make clean

echo "--- :construction: Running make"
make

echo "--- :package: Packaging RPM"
./scripts/fpm-build-rpm.sh