{-# LANGUAGE OverloadedStrings #-}

-- | X11 setup and settings.
module X11 (
  x11Module,
) where

import Common
import Component
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.YAML
import Manages
import Modules
import Packages
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.Process

data DisplayConfig = DisplayConfig
  { scalingFactor :: !Int
  , theme :: !T.Text
  , iconTheme :: !T.Text
  , cursorTheme :: !T.Text
  }
  deriving (Show)

instance FromYAML DisplayConfig where
  parseYAML :: Node Pos -> Parser DisplayConfig
  parseYAML = withMap "display-config" $ \m ->
    DisplayConfig
      <$> (m .:? "Scaling-Factor" .!= 1)
      <*> (m .: "Theme")
      <*> (m .: "Icon-Theme")
      <*> (m .: "Cursor-Theme")

loadDisplayCfg :: ManageEnv -> IO DisplayConfig
loadDisplayCfg mEnv@ManageEnv{..} = loadConfig mEnv "display-config.yaml" (readYAMLFile userError)

data SettingsValue = SetFlag !Bool | SetInt !Int | SetText !T.Text

xresourcesCfg :: DisplayConfig -> [(T.Text, SettingsValue)]
xresourcesCfg DisplayConfig{..} =
  [ -- Font rendering
    ("Xft.hinting", SetFlag True)
  , ("Xft.hintstyle", SetText "hintslight")
  , ("Xft.antialias", SetFlag True)
  , ("Xft.rgba", SetText "rgb")
  , -- HiDPI
    ("Xft.dpi", SetInt (96 * scalingFactor))
  ]
xresourcesText :: [(T.Text, SettingsValue)] -> T.Text
xresourcesText cfg = T.unlines $ do
  (field, value) <- cfg
  pure $ field <> ": " <> valueAsText value
  where
    valueAsText = \case
      SetFlag flag -> if flag then "true" else "false"
      SetInt i -> T.pack (show i)
      SetText txt -> txt -- .Xresources does not require quote here

xsettingsConf :: DisplayConfig -> [(T.Text, SettingsValue)]
xsettingsConf DisplayConfig{..} =
  [ -- Font rendering
    ("Xft/Hinting", SetInt 1)
  , ("Xft/HintStyle", SetText "hintslight")
  , ("Xft/Antialias", SetInt 1)
  , ("Xft/RGBA", SetText "rgb")
  , -- HiDPI
    ("Gdk/UnscaledDPI", SetInt 98304)
  , ("Gdk/WindowScalingFactor", SetInt scalingFactor)
  , -- Theming
    ("Net/ThemeName", SetText theme)
  , ("Net/IconThemeName", SetText iconTheme)
  , ("Gtk/CursorThemeName", SetText cursorTheme)
  ]
xsettingsText :: [(T.Text, SettingsValue)] -> T.Text
xsettingsText cfg = T.unlines $ do
  (field, value) <- cfg
  pure $ field <> " " <> valueAsText value
  where
    valueAsText = \case
      SetFlag flag -> T.pack (show flag)
      SetInt i -> T.pack (show i)
      SetText txt -> "\"" <> txt <> "\""

x11Module :: Component ModuleMode
x11Module = deps <> xresources <> xsettingsd <> xsetup
  where
    deps = ofDependencies [AsPackage "libxss", AsPackage "xmonad", AsPackage "xsettingsd", AsPackage "xsetroot"]
    xresources = ofHandle $ \mEnv@ManageEnv{..} -> \case
      CustomInstall -> do
        logger "[X11] Installing X-resources..."
        displayCfg <- loadDisplayCfg mEnv
        T.writeFile (xresourcesPath home) $ xresourcesText (xresourcesCfg displayCfg)
      CustomRemove -> do
        logger "You may remove installed X-resources config %s." (xresourcesPath home)
      InvokeOn Start -> do
        logger "[X11] Reflect X-resources."
        callProcess "xrdb" ["-merge", xresourcesPath home]
    xresourcesPath home = home </> ".Xresources"

    xsettingsd = ofHandle $ \mEnv@ManageEnv{..} -> \case
      CustomInstall -> do
        logger "[X11] Installing X settings..."
        displayCfg <- loadDisplayCfg mEnv
        -- Need to create folder first
        createDirectoryIfMissing True (home </> ".config" </> "xsettingsd")
        T.writeFile (xsettingsPath home) $ xsettingsText (xsettingsConf displayCfg)
      CustomRemove -> do
        logger "You may remove installed xsettingsd config %s." (xsettingsPath home)
      InvokeOn Start -> do
        logger "[X11] Running XSettingsd for X settings."
        _ <- spawnProcess "xsettingsd" []
        -- Workaround for GTK4 apps reaching for GTK_THEME.
        DisplayConfig{theme} <- loadDisplayCfg mEnv
        -- ? Do we need to call dbus-update-activation-environment?
        setServiceEnv "GTK_THEME" (T.unpack theme)
        setServiceEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1" -- HiDPI Scales for QT
        setServiceEnv "QT_QPA_PLATFORMTHEME" "qt5ct"
    xsettingsPath home = home </> ".config" </> "xsettingsd" </> "xsettingsd.conf"

    xsetup = ofHandle $ \ManageEnv{..} -> \case
      InvokeOn Start -> do
        callProcess "xrandr" []
        callProcess "xsetroot" ["-cursor_name", "left_ptr"]
      _ -> pure ()
