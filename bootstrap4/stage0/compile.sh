#!/bin/sh
set -e

bun $(dirname $0)/transpiler.ts "$@"
