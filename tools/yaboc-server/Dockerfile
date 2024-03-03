FROM golang:1-bullseye

ADD tools/yaboc-server /src
WORKDIR /src

RUN go build -o yaboc-server


FROM rust:1-bullseye

ADD . /src

RUN apt-get update && apt-get install -y \
    llvm-16-dev libpolly-16-dev libzstd-dev

WORKDIR /src

RUN cargo build --release


FROM emscripten/emsdk:3.1.54

COPY --from=0 /src/yaboc-server /usr/local/bin/yaboc-server
COPY --from=1 /src/target/release/yaboc /usr/local/bin/yaboc
COPY tools/yaboc-server/build.sh /usr/local/bin/build.sh
RUN apt-get update && apt-get install -y libffi7
ADD ./lib /src
ENV YABO_LIB_PATH=/src

WORKDIR /home/emscripten
USER emscripten

CMD bash -c '. /emsdk/emsdk_env.sh && "/usr/local/bin/yaboc-server" "/usr/local/bin/build.sh"'