#!/bin/sh
# shellcheck disable=SC2064
app_id="xmonad-manage"
local_bin="/usr/local/bin"
local_config="/usr/local/etc"
local_share="/usr/local/share"

cd "$( dirname "$0" )" || exit

# Make it as a temporary file
tmpdir=$( mktemp -d "${TMPDIR:-/tmp/}$(basename "$0").XXXXXXXXXXXX" )
trap "rm -rf $tmpdir" EXIT # $tmpdir should be expanded here

# Statically built for consistency
cabal install "exe:$app_id" --overwrite-policy=always \
  --disable-executable-dynamic --install-method=copy --installdir="$tmpdir"

# Installation
echo "Installing binary to $local_bin/$app_id..."
sudo mv "$tmpdir/$app_id" "$local_bin"

if [ -d "$local_config/$app_id" ]; then
  echo "Configuration found in $local_config/$app_id."
elif [ -d "config" ]; then
  echo "Copying existing configuration onto $local_config/$app_id..."
  sudo cp -rT "config" "$local_config/$app_id"
else
  echo "Copying base configuration onto $local_config/$app_id..."
  sudo cp -rT "baseconfig" "$local_config/$app_id"
fi

echo "Installing data files into $local_share/$app_id..."
sudo cp -rT --no-preserve=mode "database" "$local_share/$app_id/database"
sudo cp -rT --no-preserve=mode "modules" "$local_share/$app_id/modules"

# Removing existing xmonad-manage in /opt/bin
if [ -e "/opt/bin/xmonad-manage" ]; then
  echo "Removing existing xmonad-manage..."
  sudo rm "/opt/bin/xmonad-manage"
fi

# Checkup run
xmonad-manage --help
echo "Done."
