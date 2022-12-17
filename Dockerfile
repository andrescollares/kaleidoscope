FROM haskell:9.4.3-buster
WORKDIR /src
COPY . .

# RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# CMD ["ghci"]
CMD ["ghc", "test_run/helloworld.hs"]
CMD ["./test_run/helloworld"]

EXPOSE 3000