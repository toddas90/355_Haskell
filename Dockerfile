FROM haskell:8.10

RUN cabal update && \
    cabal v2-install --lib HUnit

# podman run -it --rm -v \`pwd\`:/work -w /work haskell-hunit

