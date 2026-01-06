FROM golang:1-trixie AS server

COPY tools/yaboc-server/go.mod tools/yaboc-server/go.sum tools/yaboc-server/yaboc-server.go /src/
WORKDIR /src

RUN go build -o yaboc-server


FROM rust:1-trixie AS compiler

RUN apt-get update && apt-get install -y cmake llvm-19-dev

COPY Cargo.toml Cargo.lock /src/yabo/
ADD crates /src/yabo/crates/

WORKDIR /src/yabo

RUN cargo build --release

FROM debian:trixie AS emscripten

ARG EMSCRIPTEN_VERSION=4.0.7

ADD https://github.com/emscripten-core/emsdk/archive/refs/tags/${EMSCRIPTEN_VERSION}.tar.gz \
    /src/

RUN apt-get update && apt-get install -y python3 xz-utils \
 && tar -xf /src/${EMSCRIPTEN_VERSION}.tar.gz -C / \
 && mv /emsdk-${EMSCRIPTEN_VERSION} /opt/emsdk \
 && cd /opt/emsdk \
 && /opt/emsdk/emsdk install $EMSCRIPTEN_VERSION \
 && /opt/emsdk/emsdk activate $EMSCRIPTEN_VERSION

FROM debian:trixie AS qt

ARG QT_VERSION=6.10.1

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
 && /src/qtbase-$QT_VERSION/configure -prefix /opt/qt6-wasm -qt-host-path /opt/qt6 -feature-thread -no-warnings-are-errors -platform wasm-emscripten -no-feature-draganddrop \
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

FROM debian:trixie-slim

RUN apt-get update && apt-get install -y python3 git llvm-19

COPY tools/yaboc-server/build.sh /opt/bin/build.sh
ADD ./lib /opt/lib
COPY --from=server /src/yaboc-server /opt/bin/yaboc-server
COPY --from=compiler /src/yabo/target/release/yaboc /opt/bin/yaboc
COPY --from=emscripten /opt/emsdk/ /opt/emsdk/
COPY --from=qt /opt/yphbt/ /opt/yphbt/
COPY ./tools/yaboc-server/index.html /opt/yphbt/index.html
COPY ./tools/yaboc-server/favicon.ico /opt/yphbt/favicon.ico

ENV YABO_LIB_PATH='/opt/lib'
ENV PATH="${PATH}:/opt/bin"
RUN mkdir /home/emscripten && chown 1000:1000 /home/emscripten
ENV HOME=/home/emscripten
RUN bash -c '. /opt/emsdk/emsdk_env.sh \
 && emcc -O3 -c /opt/lib/rt.c -fvisibility=hidden -o /opt/lib/rt.o'


USER 1000:1000
# initialize emcc's cache by running once
RUN bash -c '. /opt/emsdk/emsdk_env.sh \
 && build.sh <(echo "export def ~main = ~") /tmp/out.wasm \
 && rm /tmp/out.wasm'

WORKDIR /tmp
EXPOSE 8000
ENV YABOCSERV_ROOT=/opt/yphbt

CMD bash -c '. /opt/emsdk/emsdk_env.sh && "/opt/bin/yaboc-server" "/opt/bin/build.sh"'
