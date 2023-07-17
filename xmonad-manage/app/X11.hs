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
import System.Directory
import System.FilePath
import System.Process
import Text.Printf

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
loadDisplayCfg mEnv = loadConfig mEnv "display-config.yaml" (readYAMLFile userError)

data SettingsValue = SetFlag !Bool | SetInt !Int | SetText !T.Text

-- TODO Review if these are all good values.
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
  [field <> ": " <> valueAsText value]
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
  [field <> " " <> valueAsText value]
  where
    valueAsText = \case
      SetFlag flag -> T.pack (show $ fromEnum flag)
      SetInt i -> T.pack (show i)
      SetText txt -> "\"" <> txt <> "\""

data ThisEnv = ThisEnv !FilePath !DisplayConfig !FilePath

x11Module :: Component ModuleMode
x11Module = xmonadDeps <> (getConfig >>> xresources <> xsettingsd) <> xsetup
  where
    xmonadDeps = ofDependencies [AsPackage "libxss", AsPackage "xmonad"]
    xresources = ofHandle handleXresources
    xsettingsd = MkComponent{dependencies = [AsPackage "xsettingsd"], handle = handleXsettings}
    xsetup =
      MkComponent
        { dependencies = [AsPackage "xsetroot"]
        , handle = \_ -> \case
            InvokeOn Start -> do
              callProcess "xrandr" []
              callProcess "xsetroot" ["-cursor_name", "left_ptr"]
            _ -> pure ()
        }
    getConfig = ofAction $ \mEnv -> do
      ThisEnv mEnv.home <$> loadDisplayCfg mEnv <*> getXdgDirectory XdgConfig "xsettingsd"

handleXresources :: ThisEnv -> Context ModuleMode -> IO ()
handleXresources (ThisEnv home displayCfg _) = \case
  Custom Install -> do
    printf "[X11] Installing X-resources...\n"
    T.writeFile xresourcesPath $ xresourcesText (xresourcesCfg displayCfg)
  Custom Remove -> do
    printf "You may remove installed X-resources config %s.\n" xresourcesPath
  InvokeOn Start -> do
    printf "[X11] Reflect X-resources.\n"
    callProcess "xrdb" ["-merge", xresourcesPath]
  where
    xresourcesPath = home </> ".Xresources"

handleXsettings :: ThisEnv -> Context ModuleMode -> IO ()
handleXsettings (ThisEnv _ displayCfg xsettingsDir) = \case
  Custom Install -> do
    printf "[X11] Installing X settings...\n"
    -- Need to create folder first
    createDirectoryIfMissing False xsettingsDir
    T.writeFile (xsettingsDir </> "xsettingsd.conf") $ xsettingsText (xsettingsConf displayCfg)
  --
  Custom Remove -> do
    printf "You may remove installed xsettingsd config at %s.\n" (xsettingsDir </> "xsettingsd.conf")
  --
  InvokeOn Start -> do
    printf "[X11] Running XSettingsd for X settings.\n"
    _ <- spawnProcess "xsettingsd" []
    setServiceEnv "GTK_THEME" displayCfg.theme
    -- Workaround for GTK4 apps reaching for GTK_THEME.
    setServiceEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1" -- HiDPI Scales for QT
    setServiceEnv "QT_QPA_PLATFORMTHEME" "qt5ct"
