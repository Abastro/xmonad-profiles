#!/bin/sh
# shellcheck disable=SC2064
cd "$( dirname "$0" )" || exit

# Make it as a temporary file
tmpdir=$( mktemp -d "${TMPDIR:-/tmp/}$(basename "$0").XXXXXXXXXXXX" )
trap "rm -rf $tmpdir" EXIT # $tmpdir should be expanded here

# Statically built for consistency
cabal install "exe:xmonad-manage" --overwrite-policy=always \
  --disable-executable-dynamic --install-method=copy --installdir="$tmpdir"

# Install directory is /opt/bin
echo "Installing to /opt/bin/xmonad-manage..."
sudo mkdir -p /opt/bin
sudo mv "$tmpdir/xmonad-manage" /opt/bin

# Checkup run
/opt/bin/xmonad-manage --help
echo "Done."
