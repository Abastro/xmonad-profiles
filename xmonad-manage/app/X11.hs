-- | X11 setup and settings.
module X11 (
  x11Module,
) where

import Common
import Component
import Control.Concurrent
import Control.Exception
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.YAML
import Manages
import Packages
import References
import System.FilePath
import System.Process
import Text.Printf

-- ? Make this a separate module for managing settings?

data DisplayConfig = DisplayConfig
  { scalingFactor :: !Int
  , theme :: !T.Text
  , iconTheme :: !T.Text
  , cursorTheme :: !T.Text
  , font :: !(Maybe T.Text)
  }
  deriving (Show)

defaultConfig =
  DisplayConfig
    { scalingFactor = 1
    , theme = T.pack "Adwaita-dark"
    , iconTheme = T.pack "Adwaita"
    , cursorTheme = T.pack "DMZ-White"
    , font = Nothing
    }

instance FromYAML DisplayConfig where
  parseYAML :: Node Pos -> Parser DisplayConfig
  parseYAML = withMap "display-config" $ \m ->
    DisplayConfig
      <$> (m .:? "Scaling-Factor" .!= 1)
      <*> (m .: "Theme")
      <*> (m .: "Icon-Theme")
      <*> (m .: "Cursor-Theme")
      <*> (m .: "Font")

loadDisplayCfg :: ManageEnv -> IO DisplayConfig
loadDisplayCfg mEnv = handle onExc $ readYAMLFile userError (mEnv.configUserDir </> "display-config.yaml")
  where
    onExc (err :: IOException) = do
      printf "[X11] IO exception while trying to load display configuration:\n"
      print err
      printf "[X11] Using the default config..."
      pure defaultConfig

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
    <> fontProp
  where
    fontProp = case font of
      Just x -> [("Gtk/FontName", SetText x)]
      Nothing -> []

xsettingsText :: [(T.Text, SettingsValue)] -> T.Text
xsettingsText cfg = T.unlines $ do
  (field, value) <- cfg
  [field <> " " <> valueAsText value]
  where
    valueAsText = \case
      SetFlag flag -> T.pack (show $ fromEnum flag)
      SetInt i -> T.pack (show i)
      SetText txt -> "\"" <> txt <> "\""

x11Module :: ComponentCat ModuleMode ManageEnv ()
x11Module = withIdentifier (UnsafeMakeID "x11") $ xmonadDeps <> (getConfig >>> xresources <> xsettingsd) <> xsetup
  where
    xmonadDeps = ofDependencies [AsPackage "libxss", AsPackage "xmonad"]
    xresources = withIdentifier (UnsafeMakeID "xresources") $ ofHandle handleXresources
    xsettingsd =
      MkComponent
        { dependencies = [AsPackage "xsettingsd"]
        , identifier = UnsafeMakeID "xsettings"
        , handle = handleXsettings
        }
    xsetup =
      MkComponent
        { dependencies = [AsPackage "xsetroot"]
        , identifier = UnsafeMakeID "xsetup"
        , handle = const handleXSetup
        }
    getConfig = ofAction loadDisplayCfg

handleXSetup :: Context ModuleMode -> IO ()
handleXSetup = \case
  InvokeOn Start -> do
    callProcess "xrandr" []
    callProcess "xsetroot" ["-cursor_name", "left_ptr"]
  _ -> pure ()

handleXresources :: DisplayConfig -> Context ModuleMode -> IO ()
handleXresources displayConfig = \case
  Custom _ -> pure ()
  InvokeOn Start -> do
    printf "[X11] Reflect X-resources.\n"
    withTemporaryDirectory $ \tmpDir -> do
      let xresourcesPath = tmpDir </> ".Xresources"
      T.writeFile xresourcesPath $ xresourcesText (xresourcesCfg displayConfig)
      callProcess "xrdb" ["-merge", xresourcesPath]

handleXsettings :: DisplayConfig -> Context ModuleMode -> IO ()
handleXsettings displayConfig = \case
  Custom _ -> pure () -- Does not write config now.
  --
  InvokeOn Start -> do
    printf "[X11] Running Xsettingsd for X settings.\n"
    forkIO $ xsettingsSender displayConfig
    -- Workaround for GTK4 apps reaching for GTK_THEME. Meh.
    setServiceEnv "GTK_THEME" displayConfig.theme
    setServiceEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1" -- HiDPI Scales for QT
    setServiceEnv "QT_QPA_PLATFORMTHEME" "qt5ct"

-- ? Note that sending SIGHUP to xsettingsd will cause to refresh configuration.
-- ? This could be used for configuration updates.

-- | The thread which "sends" configuration to xsettingsd.
-- This needs to be alive somehow, soo.. :shrug:
xsettingsSender :: DisplayConfig -> IO ()
xsettingsSender displayConfig = withTemporaryDirectory $ \tmpDir -> do
  let xsettingsdConfig = tmpDir </> "xsettingsd.conf"
  T.writeFile xsettingsdConfig $ xsettingsText (xsettingsConf displayConfig)
  -- Need to be checked if this correctly hangs for us
  callProcess "xsettingsd" ["-c", xsettingsdConfig]
