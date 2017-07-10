#!/bin/bash

set -x
set -e

buildkite-agent artifact download "*.rpm" .
mv *.rpm /repo/development/CentOS_7/kazoo/x86_64/RPMS
# createrepo --update /repo/development/CentOS_7/kazoo/x86_64/