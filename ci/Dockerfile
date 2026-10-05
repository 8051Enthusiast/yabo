FROM rust:1-trixie
ARG LLVM_VERSION=22
ADD ci/apt.llvm.org.asc /etc/apt/trusted.gpg.d/apt.llvm.org.asc
RUN echo "deb [signed-by=/etc/apt/trusted.gpg.d/apt.llvm.org.asc] http://apt.llvm.org/trixie/ llvm-toolchain-trixie-$LLVM_VERSION main" > /etc/apt/sources.list.d/llvm.list
RUN apt-get update && apt-get install -y cmake lsb-release gnupg llvm-$LLVM_VERSION lld clang nodejs docker.io libmimalloc3
ARG WASI_VERSION=34
ARG TARGETARCH
RUN if [ ${TARGETARCH} = amd64 ]; then WASI_ARCH=x86_64; else WASI_ARCH=${TARGETARCH}; fi \
 && wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_VERSION}/wasi-sdk-${WASI_VERSION}.0-${WASI_ARCH}-linux.tar.gz \
 && mkdir /wasi \
 && tar -xvf wasi-sdk-${WASI_VERSION}.0-${WASI_ARCH}-linux.tar.gz --strip-components=1 -C /wasi
ARG WASMTIME_VERSION=49.0.2
RUN cargo install wasmtime-cli --version=${WASMTIME_VERSION}
