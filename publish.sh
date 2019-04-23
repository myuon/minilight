#!/bin/sh
set -e

if [ $# -ne 1 ]; then
  echo "No version specified" 1>&2
  exit 1
fi
cabal new-sdist

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal new-haddock --builddir="$dir" --haddock-for-hackage --haddock-option=--hyperlinked-source && \
cabal upload --publish dist-newstyle/sdist/minilight-$1.tar.gz && \
cabal upload --publish -d $dir/minilight-$1-docs.tar.gz
