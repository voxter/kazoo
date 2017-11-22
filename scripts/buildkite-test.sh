#!/bin/bash

set -x
set -e

CHANGED='git --no-pager diff --name-only HEAD origin/4.1-develop -- applications core'

echo "--- Changed"
echo $($CHANGED)

echo "--- Script state-of-docs"
./scripts/state-of-docs.sh || true

echo "--- Script code_checks"
./scripts/code_checks.bash $($CHANGED)

echo "--- Make fmt"
if [[ ! -z "$($CHANGED)" ]]; then
  TO_FMT="$(echo $($CHANGED))" make fmt
fi

echo "--- Make deps"
make deps

echo "--- Make compile-test"
make compile-test

echo "--- Make eunit"
make eunit

echo "--- Make clean"
make clean

echo "--- Make"
make

echo "--- Make code_checks"
make code_checks

echo "--- Script validate-js"
./scripts/validate-js.sh $($CHANGED)

echo "--- Make apis"
make apis

# echo "--- Make docs"
# make docs

echo "--- Make validate-schemas"
make validate-schemas

echo "--- Make validate-swagger"
set +e
make validate-swagger
set -e

echo "--- ??"
if [[ 0 -ne `git status --porcelain | wc -l` ]]; then
  echo Unstaged changes!
  git status --porcelain
  git --no-pager diff
  echo 'Maybe try `make apis` and see if that fixes anything ;)'
  exit 1
fi

echo "--- Swagger Tools"
set +e
time swagger-tools validate applications/crossbar/priv/api/swagger.json
set -e

echo "--- Make xref"
make xref

echo "--- Make sup_completion"
make sup_completion

echo "--- Make dialyze"
if [[ ! -z "$($CHANGED)" ]]; then
  make build-plt
  TO_DIALYZE="$(echo $($CHANGED))" make dialyze
fi

echo "--- Make sdks"
make elvis

echo "--- Make sdks"
# made sdks

echo "--- Make build-ci-release"
make build-ci-release

echo "--- Script check-release-startup"
echo "Output being sent to check-release-startup.buildkite.log, chcek build artifacts for log."
./scripts/check-release-startup.sh > check-release-startup.buildkite.log
