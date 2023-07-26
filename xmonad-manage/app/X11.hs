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
  , cornerRadius :: !Int
  , shadow :: !Bool
  , fading :: !Bool
  }
  deriving (Show)

defaultConfig =
  DisplayConfig
    { scalingFactor = 1
    , theme = T.pack "Adwaita-dark"
    , iconTheme = T.pack "Adwaita"
    , cursorTheme = T.pack "DMZ-White"
    , font = Nothing
    , cornerRadius = 0
    , shadow = True
    , fading = True
    }

instance FromYAML DisplayConfig where
  parseYAML :: Node Pos -> Parser DisplayConfig
  parseYAML = withMap "display-config" $ \m ->
    DisplayConfig
      <$> (m .:? "Scaling-Factor" .!= 1)
      <*> (m .: "Theme")
      <*> (m .: "Icon-Theme")
      <*> (m .: "Cursor-Theme")
      <*> (m .:? "Font")
      <*> (m .:? "Corner-Radius" .!= 0)
      <*> (m .:? "Shadow" .!= True)
      <*> (m .:? "Fading" .!= False)

loadDisplayCfg :: ManageEnv -> IO DisplayConfig
loadDisplayCfg mEnv = handle onExc $ readYAMLFile userError (mEnv.configUserDir </> "display-config.yaml")
  where
    onExc (err :: IOException) = do
      printf "[X11] IO exception while trying to load display configuration:\n"
      print err
      printf "[X11] Using the default config..."
      pure defaultConfig

data SettingsValue = SetFlag !Bool | SetInt !Int | SetText !T.Text | SetFloat !Float | SetTextList [T.Text]

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
      SetFlag flag -> T.pack (show flag)
      SetInt i -> T.pack (show i)
      SetText txt -> txt -- .Xresources does not require quote here
      SetFloat _ -> error "unsupported"
      SetTextList _ -> error "unsupported"

xsettingsConf :: DisplayConfig -> [(T.Text, SettingsValue)]
xsettingsConf DisplayConfig{..} =
  [ -- Font rendering
    ("Xft/Hinting", SetFlag True)
  , ("Xft/HintStyle", SetText "hintslight")
  , ("Xft/Antialias", SetFlag True)
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
      SetText txt -> T.pack (show txt)
      SetFloat _ -> error "unsupported"
      SetTextList _ -> error "unsupported"

data X11Env = X11Env !FilePath !DisplayConfig

x11Module :: ComponentCat ModuleMode ManageEnv ()
x11Module =
  withIdentifier (UnsafeMakeID "x11") $
    xmonadDeps
      <> (x11Env >>> xresources <> xsettingsd <> picomSettings)
      <> xsetup
  where
    xmonadDeps = ofDependencies [AsPackage "libxss", AsPackage "xmonad"]
    xresources = withIdentifier (UnsafeMakeID "xresources") $ ofHandle handleXresources
    xsettingsd =
      MkComponent
        { dependencies = [AsPackage "xsettingsd"]
        , identifier = UnsafeMakeID "xsettings"
        , handle = handleXsettings
        }
    picomSettings = withIdentifier (UnsafeMakeID "") $ ofHandle feedPicomSettings
    xsetup =
      MkComponent
        { dependencies = [AsPackage "xsetroot"]
        , identifier = UnsafeMakeID "xsetup"
        , handle = const handleXSetup
        }
    x11Env = ofAction $ \mEnv -> X11Env mEnv.temporaryDir <$> loadDisplayCfg mEnv

handleXSetup :: Context ModuleMode -> IO ()
handleXSetup = \case
  InvokeOn Start -> do
    callProcess "xrandr" []
    callProcess "xsetroot" ["-cursor_name", "left_ptr"]
  _ -> pure ()

handleXresources :: X11Env -> Context ModuleMode -> IO ()
handleXresources (X11Env temporaryDir displayConfig) = \case
  Custom _ -> pure ()
  InvokeOn Start -> do
    printf "[X11] Reflect X-resources.\n"
    let xresourcesPath = temporaryDir </> ".Xresources"
    T.writeFile xresourcesPath $ xresourcesText (xresourcesCfg displayConfig)
    callProcess "xrdb" ["-merge", xresourcesPath]

-- ? Note that sending SIGHUP to xsettingsd will cause to refresh configuration.
-- ? This could be used for configuration updates.

handleXsettings :: X11Env -> Context ModuleMode -> IO ()
handleXsettings (X11Env temporaryDir displayConfig) = \case
  Custom _ -> pure () -- Does not write config now.
  --
  InvokeOn Start -> do
    printf "[X11] Running Xsettingsd for X settings.\n"
    forkIO $ do
      let xsettingsdConfig = temporaryDir </> "xsettingsd.conf"
      T.writeFile xsettingsdConfig $ xsettingsText (xsettingsConf displayConfig)
      callProcess "xsettingsd" ["-c", xsettingsdConfig]
    -- Workaround for GTK4 apps reaching for GTK_THEME. Meh.
    setServiceEnv "GTK_THEME" displayConfig.theme
    setServiceEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1" -- HiDPI Scales for QT
    setServiceEnv "QT_QPA_PLATFORMTHEME" "qt5ct"

picomConfig :: DisplayConfig -> [(T.Text, SettingsValue)]
picomConfig DisplayConfig{..} =
  [ ("backend", SetText "glx") -- xrender backend is way less performant.
  , ("shadow", SetFlag True)
  , ("detect-rounded-corners", SetFlag True)
  , ("frame-opacity", SetFloat 0.8)
  , ("corner-radius", SetInt cornerRadius)
  , -- Fading settings
    ("fading", SetFlag fading)
  , ("fade-in-step", SetFloat 0.02)
  , ("fade-out-step", SetFloat 0.02)
  , ("fade-delta", SetInt 4)
  ]

picomWintypes :: DisplayConfig -> [(T.Text, [(T.Text, SettingsValue)])]
picomWintypes DisplayConfig{..} =
  [
    ( "tooltip"
    ,
      [ ("fade", SetFlag fading)
      , ("shadow", SetFlag True)
      , ("opacity", SetFloat 0.75)
      , ("full-shadow", SetFlag False)
      ]
    )
  , ("dock", [("shadow", SetFlag False), ("clip-shadow-above", SetFlag True)])
  , ("dnd", [("shadow", SetFlag False)])
  , ("popup-menu", [("opacity", SetFloat 0.8)])
  , ("dropdown-menu", [("opacity", SetFloat 0.8)])
  ]

picomWholeText :: [(T.Text, SettingsValue)] -> [(T.Text, [(T.Text, SettingsValue)])] -> T.Text
picomWholeText cfg winTypes = T.unlines [picomText cfg, winTypePart]
  where
    winTypePart = T.unlines ["wintypes:", "{", winTypeBody, "};"]
    winTypeBody = T.unlines $ do
      (winType, typCfg) <- winTypes
      [winType <> " = " <> "{" <> picomText typCfg <> "};"]

picomText :: [(T.Text, SettingsValue)] -> T.Text
picomText cfg = T.unlines $ do
  (field, value) <- cfg
  [field <> " = " <> valueAsText value <> ";"]
  where
    valueAsText = \case
      SetFlag f -> T.pack (show f)
      SetInt i -> T.pack (show i)
      SetText txt -> T.pack (show txt)
      SetFloat fl -> T.pack (show fl)
      SetTextList txtList -> T.pack (show txtList)

feedPicomSettings :: X11Env -> Context ModuleMode -> IO ()
feedPicomSettings (X11Env temporaryDir displayConfig) = \case
  Custom _ -> pure ()
  InvokeOn Start -> do
    printf "[X11] Writing picom settings in case picom would use it...\n"
    let picomCfgPath = temporaryDir </> "picom.conf"
    T.writeFile picomCfgPath $ picomWholeText (picomConfig displayConfig) (picomWintypes displayConfig)
