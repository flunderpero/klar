#!/bin/sh
set -e

bun $(dirname $0)/compiler.ts "$@"
