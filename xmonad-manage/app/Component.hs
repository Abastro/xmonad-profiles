module Component (
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

data Context a = CustomInstall | CustomRemove | InvokeOn a

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
  logger "Installing dependencies.."
  installPackages mEnv pkgDb distro cond dependencies
  logger "Running custom installation process.."
  handle mEnv CustomInstall

--  * When removal is being implemented, both distro and package database is needed.
remove :: ManageEnv -> Component a -> IO ()
remove mEnv@ManageEnv{..} MkComponent{..} = do
  logger "Removing dependencies is not yet implemented."
  logger "Packages %s was installed." (show $ packageName <$> dependencies)
  logger "Running custom removal process..."
  handle mEnv CustomRemove

invoke :: ManageEnv -> a -> Component a -> IO ()
invoke mEnv ctxt MkComponent{..} = do
  handle mEnv (InvokeOn ctxt)

-- | Make a component from script, which uses given script executor.
fromScript ::
  (Enum a, Bounded a) =>
  (ManageEnv -> FilePath -> Context a -> IO ()) ->
  Maybe FilePath ->
  Maybe FilePath ->
  (a -> FilePath) ->
  Component a
fromScript executor installScript removeScript invokeScript =
  MkComponent
    { dependencies = []
    , handle = \mEnv -> \case
        CustomInstall -> do
          for_ installScript $ \inst -> do
            setToExecutable inst
            executor mEnv inst CustomInstall
          for_ [minBound .. maxBound] $ \ctxt -> do
            setToExecutable (invokeScript ctxt)
        CustomRemove -> do
          for_ removeScript $ \rm -> do
            setToExecutable rm
            executor mEnv rm CustomRemove
        InvokeOn ctxt -> do
          executor mEnv (invokeScript ctxt) (InvokeOn ctxt)
    }

ofHandle :: (ManageEnv -> Context a -> IO ()) -> Component a
ofHandle handle = MkComponent{dependencies = [], handle}

ofDependencies :: [Package] -> Component a
ofDependencies dependencies = MkComponent{dependencies, handle = mempty}