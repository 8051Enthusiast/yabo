FROM golang:1-bookworm AS server

ADD tools/yaboc-server /src
WORKDIR /src

RUN go build -o yaboc-server


FROM rust:1-bookworm AS compiler

RUN apt-get update && apt-get install -y cmake

ARG LLVM_VERSION=18.1.4

ADD https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/llvm-project-${LLVM_VERSION}.src.tar.xz \
    /src/
RUN --mount=type=tmpfs,target=/build \
    cd /build \
 && tar -xf /src/llvm-project-${LLVM_VERSION}.src.tar.xz -C /src \
 && cmake -DLLVM_ENABLE_PROJECTS=polly -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="WebAssembly" /src/llvm-project-${LLVM_VERSION}.src/llvm \
 && make -j$(nproc) \
 && make install \
 && rm -rf /src/llvm-project-${LLVM_VERSION}.src

COPY Cargo.toml Cargo.lock /src/yabo/
ADD crates /src/yabo/crates/

WORKDIR /src/yabo

RUN cargo build --release

FROM debian:bookworm AS emscripten

ARG EMSCRIPTEN_VERSION=3.1.58

ADD https://github.com/emscripten-core/emsdk/archive/refs/tags/${EMSCRIPTEN_VERSION}.tar.gz \
    /src/

RUN apt-get update && apt-get install -y python3 xz-utils \
 && tar -xf /src/${EMSCRIPTEN_VERSION}.tar.gz -C / \
 && mv /emsdk-${EMSCRIPTEN_VERSION} /opt/emsdk \
 && cd /opt/emsdk \
 && /opt/emsdk/emsdk install $EMSCRIPTEN_VERSION \
 && /opt/emsdk/emsdk activate $EMSCRIPTEN_VERSION

FROM debian:bookworm AS qt

ARG QT_VERSION=6.7.0

ADD https://github.com/qt/qtbase/archive/refs/tags/v$QT_VERSION.zip /src/
RUN apt-get update && apt-get install -y build-essential cmake unzip libgl1-mesa-dev
RUN unzip /src/v$QT_VERSION.zip -d /src
RUN --mount=type=tmpfs,target=/build \
    cd /build \
 && /src/qtbase-$QT_VERSION/configure -prefix /opt/qt6 -nomake examples -nomake tests \
 && cmake --build . -t qtbase --parallel $(nproc)\
 && cmake --install . --prefix /opt/qt6

COPY --from=emscripten /opt/emsdk/ /opt/emsdk/

RUN apt-get update && apt-get install -y python3 git
COPY tools/yaboc-server/qt.patch /src/qt.patch

RUN --mount=type=tmpfs,target=/build bash -c "\
    . /opt/emsdk/emsdk_env.sh \
 && patch -p1 -d /src/qtbase-$QT_VERSION < /src/qt.patch \
 && cd /build \
 && /src/qtbase-$QT_VERSION/configure -prefix /opt/qt6-wasm -qt-host-path /opt/qt6 -feature-thread -no-warnings-are-errors -platform wasm-emscripten \
 && cmake --build . -t qtbase --parallel $(nproc)\
 && cmake --install . --prefix /opt/qt6-wasm"

ADD https://github.com/ColinDuquesnoy/QDarkStyleSheet/archive/refs/tags/v3.2.2.tar.gz /src/
RUN tar -xf /src/v3.2.2.tar.gz -C /src

COPY tools/ylliab/* /src/tools/ylliab/
COPY include/yabo/* /src/include/yabo/

RUN --mount=type=tmpfs,target=/build bash -c '\
    . /opt/emsdk/emsdk_env.sh \
 && export EMCC_FORCE_STDLIBS=1 \
 && cd /build \
 && /opt/qt6-wasm/bin/qt-cmake -DCMAKE_BUILD_TYPE=Release /src/tools/ylliab \
 && /opt/qt6-wasm/bin/qt-cmake -DDARK_STYLE_QRC_PATH=/src/QDarkStyleSheet-3.2.2/qdarkstyle/dark/darkstyle.qrc /src/tools/yphbt . \
 && make -j $(nproc) yphbt && mkdir -p /opt/yphbt \
 && cp yphbt.js yphbt.html yphbt.wasm qtlogo.svg qtloader.js /opt/yphbt'

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y python3 git

COPY tools/yaboc-server/build.sh /opt/bin/build.sh
ADD ./lib /opt/lib
COPY --from=server /src/yaboc-server /opt/bin/yaboc-server
COPY --from=compiler /src/yabo/target/release/yaboc /opt/bin/yaboc
COPY --from=emscripten /opt/emsdk/ /opt/emsdk/
COPY --from=qt /opt/yphbt/ /opt/yphbt/
COPY ./tools/yaboc-server/index.html /opt/yphbt/index.html

ENV YABO_LIB_PATH=/opt/lib
RUN mkdir /home/emscripten && chown 1000:1000 /home/emscripten
ENV HOME=/home/emscripten
RUN bash -c '. /opt/emsdk/emsdk_env.sh \
 && emcc -O3 -c /opt/lib/rt.c -fvisibility=hidden -o /opt/lib/rt.o'


USER 1000:1000
# initialize emcc's cache by running once
RUN bash -c '. /opt/emsdk/emsdk_env.sh \
 && /opt/bin/build.sh <(echo "export def *main = ~") /dev/null'

WORKDIR /tmp
EXPOSE 8000
ENV YABOCSERV_ROOT=/opt/yphbt

CMD bash -c '. /opt/emsdk/emsdk_env.sh && "/opt/bin/yaboc-server" "/opt/bin/build.sh"'
