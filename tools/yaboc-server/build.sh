#!/bin/sh
set -eu
tmp_file="$(mktemp)"
cleanup() {
    rm "$tmp_file"
}
trap cleanup EXIT

timeout 15s /opt/bin/yaboc --target=wasm32-unknown-emscripten --emit=object "$1" "$tmp_file"
emcc -shared -sSIDE_MODULE=1 -pthread "$YABO_RUNTIME" "$tmp_file" -o "$2"
