#!/bin/bash

set -euo pipefail

buildkite-agent artifact download "*.rpm" .
mv *.rpm /repo/development/CentOS_7/x86_64/RPMS
createrepo --update /repo/development/CentOS_7/x86_64/