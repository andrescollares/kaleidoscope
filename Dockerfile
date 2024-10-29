FROM haskell:8.10.7-buster

# Install tools
RUN apt-get -y update; \
    apt-get install -y bash vim nano

# Install LLVM
## Install dependencies
RUN apt-get -qq update; \
    apt-get install -qqy --no-install-recommends \
    gnupg2 wget ca-certificates apt-transport-https \
    autoconf automake cmake dpkg-dev file make patch libc6-dev

## Set repository key
RUN wget -nv -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -

## Install
RUN echo "deb http://apt.llvm.org/buster/ llvm-toolchain-buster-12 main" > /etc/apt/sources.list.d/llvm.list;

RUN apt-get -qq update && apt-get install -qqy -t llvm-toolchain-buster-12 clang-12 clang-tidy-12 clang-format-12 lld-12

RUN for f in /usr/lib/llvm-12/bin/*; do ln -sf "$f" /usr/bin; done && rm -rf /var/lib/apt/lists/*

# Install Haskell Language Server
RUN wget https://github.com/haskell/haskell-language-server/releases/download/2.2.0.0/haskell-language-server-2.2.0.0-x86_64-linux-deb10.tar.xz && tar xf haskell-language-server-2.2.0.0-x86_64-linux-deb10.tar.xz && \
    cd haskell-language-server-2.2.0.0 && make && make install

# Update cabal
RUN cabal update && cabal install cabal-install

# Compile the C sources
COPY /kaleidoscope/src/StdLib/cbits /kaleidoscope/src/StdLib/cbits

WORKDIR /kaleidoscope/src/StdLib/cbits
# put the shared object file (.so) under /usr/lib -> this is passed to ghc options in the cabal file
RUN gcc -fPIC -shared -o /usr/lib/stdlib.so io.c list.c

WORKDIR /kaleidoscope

# Add just the .cabal file to capture dependencies
COPY /kaleidoscope/cabal.project /kaleidoscope/kaleidoscope-fing.cabal ./

RUN cabal build --only-dependencies -j8

COPY /kaleidoscope /kaleidoscope
RUN cabal install

CMD ["bash"]
