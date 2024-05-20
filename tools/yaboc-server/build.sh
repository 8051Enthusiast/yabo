#!/bin/sh
set -eu
tmp_file="$(mktemp)"
cleanup() {
    rm "$tmp_file"
}
trap cleanup EXIT

case "$1" in
    --target-features=*) 
        features="$1"
        shift
        ;;
    *)
        features=""
        ;;
esac

timeout 15s yaboc --target=wasm32-unknown-emscripten --emit=object $features "$1" "$tmp_file"
emcc -shared -sSIDE_MODULE=1 -pthread "$YABO_LIB_PATH/rt.o" "$tmp_file" -o "$2"
