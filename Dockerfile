FROM amd64/haskell:9.2.5-buster
WORKDIR /src
COPY . .


# instalar llvm

RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive \
  apt-get upgrade \
  -o Dpkg::Options::=--force-confold \
  -o Dpkg::Options::=--force-confdef \
  -y --allow-downgrades --allow-remove-essential --allow-change-held-packages

RUN apt-get install llvm-13-dev \
  -o Dpkg::Options::=--force-confold \
  -o Dpkg::Options::=--force-confdef \
  -y --allow-downgrades --allow-remove-essential --allow-change-held-packages

# correr hello world

# esto da "exec ./test_run/helloworld: exec format error"
# pero si se corre con docker run -it kaleidoscope-amd64 a mano da bien

# CMD ["ghc", "test_run/helloworld.hs"]
# CMD ["./test_run/helloworld"]

EXPOSE 3000