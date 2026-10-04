#!/bin/sh
repo="$(realpath $(dirname $0))"
export YABO_LIB_PATH="$repo/../lib"
echo "$YABO_LIB_PATH"
"$repo"/../ybtest.py
