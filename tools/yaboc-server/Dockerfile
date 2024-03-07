FROM golang:1-bookworm

ADD tools/yaboc-server /src
WORKDIR /src

RUN go build -o yaboc-server


FROM rust:1-bookworm

RUN apt-get update && apt-get install -y cmake

ADD https://github.com/llvm/llvm-project/releases/download/llvmorg-17.0.6/llvm-project-17.0.6.src.tar.xz \
    /src/
RUN --mount=type=tmpfs,target=/build \
    cd /build \
 && tar -xf /src/llvm-project-17.0.6.src.tar.xz -C /src \
 && cmake -DLLVM_ENABLE_PROJECTS=polly -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="X86;WebAssembly" /src/llvm-project-17.0.6.src/llvm \
 && make -j$(nproc) \
 && make install \
 && rm -rf /src/llvm-project-17.0.6.src

COPY Cargo.toml Cargo.lock /src/yabo/
ADD crates /src/yabo/crates/

WORKDIR /src/yabo

RUN cargo build --release

FROM debian:bookworm

ADD https://github.com/qt/qtbase/archive/refs/tags/v6.6.2.zip /src/
RUN apt-get update && apt-get install -y build-essential cmake unzip libgl1-mesa-dev
RUN unzip /src/v6.6.2.zip -d /src
RUN --mount=type=tmpfs,target=/build \
    cd /build \
 && /src/qtbase-6.6.2/configure -prefix /opt/qt6 -nomake examples -nomake tests \
 && cmake --build . -t qtbase \
 && cmake --install . --prefix /opt/qt6

ARG EMSCRIPTEN_VERSION=3.1.54

ADD https://github.com/emscripten-core/emsdk/archive/refs/tags/${EMSCRIPTEN_VERSION}.tar.gz \
    /src/

RUN apt-get update && apt-get install -y python3 xz-utils \
 && tar -xf /src/${EMSCRIPTEN_VERSION}.tar.gz -C / \
 && mv /emsdk-${EMSCRIPTEN_VERSION} /opt/emsdk \
 && /opt/emsdk/emsdk install ${EMSCRIPTEN_VERSION} \
 && /opt/emsdk/emsdk activate ${EMSCRIPTEN_VERSION}

RUN --mount=type=tmpfs,target=/build bash -c '\
    . /opt/emsdk/emsdk_env.sh \
 && cd /build \
 && /src/qtbase-6.6.2/configure -prefix /opt/qt6-wasm -qt-host-path /opt/qt6 -feature-thread -no-warnings-are-errors -platform wasm-emscripten \
 && cmake --build . -t qtbase --parallel $(nproc)\
 && cmake --install . --prefix /opt/qt6-wasm'

ADD https://github.com/ColinDuquesnoy/QDarkStyleSheet/archive/refs/tags/v3.2.2.tar.gz /src/
RUN tar -xf /src/v3.2.2.tar.gz -C /src

COPY tools/ylliab/* /src/tools/ylliab/
COPY include/yabo/* /src/include/yabo/

RUN --mount=type=tmpfs,target=/build bash -c '\
    . /opt/emsdk/emsdk_env.sh \
 && cd /build \
 && /opt/qt6-wasm/bin/qt-cmake -DCMAKE_BUILD_TYPE=Release /src/tools/ylliab \
 && /opt/qt6-wasm/bin/qt-cmake -DDARK_STYLE_QRC_PATH=/src/QDarkStyleSheet-3.2.2/qdarkstyle/dark/darkstyle.qrc /src/tools/yphbt . \
 && make -j $(nproc) yphbt && mkdir -p /opt/yphbt \
 && cp yphbt.js yphbt.worker.js yphbt.wasm qtlogo.svg qtloader.js /opt/yphbt'

FROM debian:bookworm-slim

ARG EMSCRIPTEN_VERSION=3.1.54

ADD https://github.com/emscripten-core/emsdk/archive/refs/tags/${EMSCRIPTEN_VERSION}.tar.gz \
    /src/
RUN apt-get update && apt-get install -y python3 xz-utils \
 && tar -xf /src/${EMSCRIPTEN_VERSION}.tar.gz -C / \
 && mv /emsdk-${EMSCRIPTEN_VERSION} /opt/emsdk \
 && /opt/emsdk/emsdk install ${EMSCRIPTEN_VERSION} \
 && /opt/emsdk/emsdk activate ${EMSCRIPTEN_VERSION} \
 && rm -rf /src 

COPY tools/yaboc-server/build.sh /opt/bin/build.sh
ADD ./lib /opt/lib
COPY --from=0 /src/yaboc-server /opt/bin/yaboc-server
COPY --from=1 /src/yabo/target/release/yaboc /opt/bin/yaboc
COPY --from=2 /opt/yphbt/* /opt/yphbt/
COPY ./tools/yaboc-server/index.html /opt/yphbt/index.html

ENV YABO_LIB_PATH=/opt/lib

# initialize emcc's cache by running once
RUN bash -c '. /opt/emsdk/emsdk_env.sh && /opt/bin/build.sh <(echo "export def *main = ~") /dev/null'

WORKDIR /tmp
USER 1000:1000
EXPOSE 8000
ENV YABOCSERV_ROOT=/opt/yphbt

CMD bash -c '. /opt/emsdk/emsdk_env.sh && "/opt/bin/yaboc-server" "/opt/bin/build.sh"'