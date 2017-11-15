#!/bin/bash

set -x
set -e

echo "--- :construction: Running make"
make

echo "--- :package: Packaging RPM"
./scripts/fpm-build-rpm.sh
