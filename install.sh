#!/bin/sh
# shellcheck disable=SC2064
app_id="xmonad-manage"
shared_bin="/usr/local/bin"
shared_config="/usr/local/etc"
shared_share="/usr/local/share"
user_config="$HOME/.config"

cd "$( dirname "$0" )" || exit

# Make it as a temporary file
tmpdir=$( mktemp -d "${TMPDIR:-/tmp/}$(basename "$0").XXXXXXXXXXXX" )
trap "rm -rf $tmpdir" EXIT # $tmpdir should be expanded here

# Statically built for consistency
cabal install "exe:$app_id" --overwrite-policy=always \
  --disable-executable-dynamic --install-method=copy --installdir="$tmpdir"

# Installation
echo "Installing binary to $shared_bin/$app_id..."
sudo mv "$tmpdir/$app_id" "$shared_bin"

if [ -d "$shared_config/$app_id" ]; then
  echo "Shared configuration found in $shared_config/$app_id."
elif [ -d "config" ]; then
  echo "Copying existing shared configuration onto $shared_config/$app_id..."
  sudo cp -rT "config/active-modules.yaml" "$shared_config/$app_id/active-modules.yaml"
else
  echo "Copying shared configuration template onto $shared_config/$app_id..."
  sudo cp -rT "config-shared" "$shared_config/$app_id"
fi

if [ -d "$user_config/$app_id" ]; then
  echo "User configuration found in $user_config/$app_id."
elif [ -d "config" ]; then
  echo "Copying existing user configuration onto $user_config/$app_id..."
  cp -rT "config/display-config.yaml" "$user_config/$app_id/display-config.yaml"
else
  echo "Copying user configuration template onto $user_config/$app_id..."
  cp -rT "config-user" "$user_config/$app_id"
fi

echo "Installing data files into $shared_share/$app_id..."
sudo cp -rT --no-preserve=mode "database" "$shared_share/$app_id/database"
sudo cp -rT --no-preserve=mode "modules" "$shared_share/$app_id/modules"

# Removing existing xmonad-manage in /opt/bin
if [ -e "/opt/bin/xmonad-manage" ]; then
  echo "Removing existing xmonad-manage..."
  sudo rm "/opt/bin/xmonad-manage"
fi

# Checkup run
xmonad-manage --help
echo "Done."
