#!/usr/bin/env bash
set -e

cabal haddock --haddock-tests \
  plutus-cbor \
  plutus-merkle-tree \
  hydra-plutus \
  hydra-tui \
  hydra-node \
  hydra-cluster

[ ! -d docs/static/haddock ] && mkdir -p docs/static/haddock

doc_indices=$(find dist-newstyle/build -name html -a -type d)

for index in ${doc_indices}; do
  echo "Copying ${index}/* to docs/static/haddock"
  cp -fr ${index}/* docs/static/haddock
done
