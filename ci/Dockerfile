FROM rust:1-trixie
ARG LLVM_VERSION=22
ADD ci/apt.llvm.org.asc /etc/apt/trusted.gpg.d/apt.llvm.org.asc
RUN echo "deb [signed-by=/etc/apt/trusted.gpg.d/apt.llvm.org.asc] http://apt.llvm.org/trixie/ llvm-toolchain-trixie-$LLVM_VERSION main" > /etc/apt/sources.list.d/llvm.list
RUN apt-get update && apt-get install -y cmake lsb-release gnupg llvm-$LLVM_VERSION lld clang nodejs docker.io
