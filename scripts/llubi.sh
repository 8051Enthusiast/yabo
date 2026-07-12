#!/bin/sh
set -eu
yaboll="$(mktemp yaboXXXXXX.ll)"
yaboprintll="$(mktemp yaboprintXXXXXX.ll)"
yaboc --llubi --emit=llvm "$1" "$yaboll"
clang-22 -ggdb -std=c23 -DLLUBI_COMPATIBLE=1 -I include \
    -DSTATIC_FILE="\"$3\"" -DSTATIC_PARSER="$2" -S -emit-llvm -o "$yaboprintll" \
    tools/yaboprint/yaboprint.c
llvm-link-22 "$yaboll" "$yaboprintll" -S
rm "$yaboprintll"
rm "$yaboll"
