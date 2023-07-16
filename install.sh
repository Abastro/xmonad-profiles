#!/bin/sh
# shellcheck disable=SC2064
cd "$( dirname "$0" )" || exit

# Make it as a temporary file
tmpdir=$( mktemp -d "${TMPDIR:-/tmp/}$(basename "$0").XXXXXXXXXXXX" )
trap "rm -rf $tmpdir" EXIT # Want to expand it here

# Statically built for consistency
cabal install "exe:xmonad-manage" --disable-executable-dynamic --install-method=copy --installdir="$tmpdir"
echo ""
"$tmpdir"/xmonad-manage update
