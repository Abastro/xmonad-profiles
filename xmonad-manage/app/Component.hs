module Component (
  SetupPhase (..),
  Context (..),
  Component (..),
  install,
  remove,
  invoke,
  fromScript,
  ofHandle,
  ofDependencies,
) where

import Common
import Data.Foldable
import Manages
import Packages
import Text.Printf

data SetupPhase = Install | Remove
  deriving (Eq, Show, Enum, Bounded)
data Context a = Custom SetupPhase | InvokeOn a

-- | A unit of installation. Can be conveniently merged.
-- When merged, former component is executed first.
data Component a = MkComponent
  { dependencies :: [Package]
  , handle :: ManageEnv -> Context a -> IO ()
  }

instance Semigroup (Component a) where
  (<>) :: Component a -> Component a -> Component a
  MkComponent dep han <> MkComponent dep' han' = MkComponent (dep <> dep') (han <> han')
instance Monoid (Component a) where
  mempty :: Component a
  mempty = MkComponent mempty mempty

install :: ManageEnv -> PkgDatabase -> ManageID -> InstallCond -> Component a -> IO ()
install mEnv@ManageEnv{..} pkgDb distro cond MkComponent{..} = do
  printf "Installing dependencies...\n"
  installPackages mEnv pkgDb distro cond dependencies
  printf "Running custom installation process...\n"
  handle mEnv (Custom Install)

--  * When removal is being implemented, both distro and package database is needed.
remove :: ManageEnv -> Component a -> IO ()
remove mEnv@ManageEnv{..} MkComponent{..} = do
  printf "Removing dependencies is not yet implemented.\n"
  printf "Packages %s was installed.\n" (show $ packageName <$> dependencies)
  printf "Running custom removal process...\n"
  handle mEnv (Custom Remove)

invoke :: ManageEnv -> a -> Component a -> IO ()
invoke mEnv ctxt MkComponent{..} = do
  handle mEnv (InvokeOn ctxt)

-- | Make a component from script, which uses given script executor.
fromScript ::
  (Enum a, Bounded a) =>
  (ManageEnv -> FilePath -> Context a -> IO ()) ->
  (SetupPhase -> Maybe FilePath) ->
  (a -> FilePath) ->
  Component a
fromScript executor setupScripts invokeScripts = ofHandle $ \mEnv -> \case
  Custom phase -> do
    case phase of
      Install -> for_ [minBound .. maxBound] $ \ctxt -> setToExecutable (invokeScripts ctxt)
      Remove -> pure ()
    for_ (setupScripts phase) $ \setup -> do
      setToExecutable setup
      executor mEnv setup (Custom Install)
  InvokeOn ctxt -> do
    executor mEnv (invokeScripts ctxt) (InvokeOn ctxt)

ofHandle :: (ManageEnv -> Context a -> IO ()) -> Component a
ofHandle handle = MkComponent{dependencies = [], handle}

ofDependencies :: [Package] -> Component a
ofDependencies dependencies = MkComponent{dependencies, handle = mempty}
