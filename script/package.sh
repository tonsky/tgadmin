#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

go build -trimpath -ldflags "-s -w" -o target/tgadmin ./cmd/tgadmin
