#!/bin/bash

set -x
set -e

ARTIFACTS_PATH=${PWD}/_rel/kazoo/buildkite-artifacts
TEST_REPORTS_PATH=${PWD}/_rel/kazoo/buildkite-test-results

mkdir -p $ARTIFACTS_PATH $TEST_REPORTS_PATH

# Don't fail if xargs exits w/ 123 due to no input (there are no changes after merge)
set +e
CHANGED=$(git --no-pager diff --name-only HEAD origin/4.3-develop -- applications core | xargs readlink -e)
set -e

echo "--- Changed"
echo $CHANGED

echo "--- Script state-of-docs"
./scripts/state-of-docs.sh || true

echo "--- Script code_checks"
./scripts/code_checks.bash $CHANGED

echo "--- Make fmt"
make fmt

echo "--- Make clean"
set +e
make clean
set -e

echo "--- Remove deps and .erlang.mk"
# Can't do this with `make clean-deps` because of Buildkite's `git clean -fxdq`
rm -rf deps
rm -rf .erlang.mk

echo "--- Make deps"
make deps

echo "--- Make compile-test"
ERLC_OPTS='-DPROPER' make compile-test

echo "--- Make eunit"
make eunit

echo "--- Make clean"
make clean

echo "--- Make"
JOBS="2" make

echo "--- Make code_checks"
make code_checks

echo "--- Make app_applications"
make app_applications

echo "--- Script validate-js"
./scripts/validate-js.sh $(find {core,applications}/*/priv/**/* -name *.json)

echo "--- Make apis"
make apis

echo "--- Make docs"
make docs

echo "--- Make validate-schemas"
make validate-schemas

echo "--- Script state-of-edoc"
./scripts/state-of-edoc.escript

echo "--- Make validate-swagger"
set +e
make validate-swagger
set -e

echo "--- Swagger Tools"
set +e
time swagger-tools validate applications/crossbar/priv/api/swagger.json
set -e

echo "--- Make xref"
make xref

echo "--- Make sup_completion"
make sup_completion

echo "--- Make dialyze"
TO_DIALYZE="$(echo $CHANGED)" make build-plt dialyze

echo "--- Make elvis"
make elvis

echo "--- Make build-ci-release"
make build-ci-release

echo "--- Check for unstaged files"
${PWD}/scripts/check-unstaged.bash

echo "--- Make release"
KAZOO_CONFIG=${PWD}/rel/ci.buildkite.ini REL="kazoo_apps" ACT="console" NODE_NAME_TYPE="-sname" make release

cp ${PWD}/rel/ci.relx.config $ARTIFACTS_PATH/
find ${PWD}/_rel/kazoo/releases -name kazoo.rel -exec cp {} $ARTIFACTS_PATH/ \;

if [[ $(grep -c -v -F 'exit with reason shutdown' ${ARTIFACTS_PATH}/log/error.log) -gt 0 ]]; then
  cat ${ARTIFACTS_PATH}/log/error.log
  exit 1
fi

make clean-release
