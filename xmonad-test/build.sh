#!/bin/sh
PROF_ID="xmonad-test"
XMONAD_EXE_PATH="$XMONAD_CACHE_DIR/$XMONAD_NAME"

cabal install "exe:$PROF_ID" \
  --installdir="$XMONAD_CACHE_DIR" --install-method=copy \
  --overwrite-policy=always
ln -sf "$XMONAD_CACHE_DIR/$PROF_ID" "$XMONAD_EXE_PATH"
