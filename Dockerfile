FROM haskell:8.2.1

RUN cabal update

RUN cabal install mtl

RUN apt-get update

RUN apt-get install -y gcc-multilib make vim nano

RUN mkdir latte

WORKDIR /latte

COPY src src

COPY lib lib

COPY Makefile latc_x86 latc ./

CMD ["/bin/bash"]
