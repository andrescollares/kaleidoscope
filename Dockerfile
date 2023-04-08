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
RUN echo "deb http://apt.llvm.org/buster/ llvm-toolchain-buster-12 main" \
        > /etc/apt/sources.list.d/llvm.list; \
    apt-get -qq update && \
    apt-get install -qqy -t llvm-toolchain-buster-12 clang-12 clang-tidy-12 clang-format-12 lld-12 && \
    for f in /usr/lib/llvm-12/bin/*; do ln -sf "$f" /usr/bin; done && \
    rm -rf /var/lib/apt/lists/*

RUN cabal update

# This keeps Cabal up to date
RUN cabal install cabal-install

# Compile the C sources (cabal can be set up to do this also, but it doesn't seem to work)
WORKDIR /

COPY /kaleidoscope/src/cbits/putchard.h /kaleidoscope/src/cbits/putchard.h
COPY /kaleidoscope/src/cbits/putchard.c /kaleidoscope/src/cbits/putchard.c

# put the shared object file (.so) under /usr/lib -> this is passed to ghc options in the cabal file
RUN gcc -fPIC -shared /kaleidoscope/src/cbits/putchard.c -o /usr/lib/putchard.so

WORKDIR /kaleidoscope

# Add just the .cabal file to capture dependencies
COPY /kaleidoscope/cabal.project /kaleidoscope/kaleidoscope-fing.cabal .
# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal configure --extra-lib-dirs=${pkgroot}/cbits

RUN cabal build --only-dependencies -j8

COPY /kaleidoscope /kaleidoscope
RUN cabal install

CMD ["bash"]
