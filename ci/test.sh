#!/bin/sh
repo="$(realpath $(dirname $0))"
export YABO_LIB_PATH="$repo/../lib"
"$repo"/../ybtest.py -c ci
