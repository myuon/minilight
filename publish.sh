#!/bin/sh
set -e

cabal new-sdist

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal new-haddock --builddir="$dir" --haddock-for-hackage --haddock-option=--hyperlinked-source && \
cabal upload --publish dist-newstyle/sdist/*.tar.gz && \
cabal upload --publish -d $dir/*-docs.tar.gz
