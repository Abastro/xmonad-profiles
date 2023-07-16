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
import Control.Applicative
import Data.Foldable
import Manages
import Packages
import Text.Printf

data SetupPhase = Install | Remove
  deriving (Eq, Show, Enum, Bounded)
data Context mode = Custom SetupPhase | InvokeOn mode

-- | A unit of installation. Can be conveniently merged.
--
-- When merged, former component is executed first.
data Component mode a = MkComponent
  { dependencies :: [Package]
  , handle :: ManageEnv -> Context mode -> IO a
  }

instance (Semigroup a) => Semigroup (Component mode a) where
  (<>) :: (Semigroup a) => Component mode a -> Component mode a -> Component mode a
  MkComponent dep han <> MkComponent dep' han' = MkComponent (dep <> dep') (han <> han')
instance (Monoid a) => Monoid (Component mode a) where
  mempty :: (Monoid a) => Component mode a
  mempty = MkComponent mempty mempty

instance Functor (Component mode) where
  fmap :: (a -> b) -> Component mode a -> Component mode b
  fmap = liftA
instance Applicative (Component mode) where
  pure :: a -> Component mode a
  pure ret = MkComponent [] (\_ _ -> pure ret)
  (<*>) :: Component mode (a -> b) -> Component mode a -> Component mode b
  f <*> x =
    MkComponent
      { dependencies = f.dependencies <> x.dependencies
      , handle = \mEnv ctxt -> f.handle mEnv ctxt <*> x.handle mEnv ctxt
      }

install :: ManageEnv -> PkgDatabase -> ManageID -> InstallCond -> Component mode () -> IO ()
install mEnv@ManageEnv{..} pkgDb distro cond MkComponent{..} = do
  printf "Installing dependencies...\n"
  installPackages mEnv pkgDb distro cond dependencies
  printf "Running custom installation process...\n"
  handle mEnv (Custom Install)

--  * When removal is being implemented, both distro and package database is needed.
remove :: ManageEnv -> Component mode () -> IO ()
remove mEnv@ManageEnv{..} MkComponent{..} = do
  printf "Removing dependencies is not yet implemented.\n"
  printf "Packages %s was installed.\n" (show $ packageName <$> dependencies)
  printf "Running custom removal process...\n"
  handle mEnv (Custom Remove)

invoke :: ManageEnv -> mode -> Component mode () -> IO ()
invoke mEnv mode MkComponent{..} = do
  handle mEnv (InvokeOn mode)

-- | Make a component from script, which uses given script executor.
fromScript ::
  (Enum mode, Bounded mode) =>
  (ManageEnv -> FilePath -> Context mode -> IO ()) ->
  (SetupPhase -> Maybe FilePath) ->
  (mode -> FilePath) ->
  Component mode ()
fromScript executor setupScripts invokeScripts = ofHandle $ \mEnv -> \case
  Custom phase -> do
    case phase of
      Install -> for_ [minBound .. maxBound] $ \mode -> setToExecutable (invokeScripts mode)
      Remove -> pure ()
    for_ (setupScripts phase) $ \setup -> do
      setToExecutable setup
      executor mEnv setup (Custom Install)
  InvokeOn ctxt -> do
    executor mEnv (invokeScripts ctxt) (InvokeOn ctxt)

ofHandle :: (ManageEnv -> Context mode -> IO ()) -> Component mode ()
ofHandle handle = MkComponent{dependencies = [], handle}

ofDependencies :: [Package] -> Component mode ()
ofDependencies dependencies = MkComponent{dependencies, handle = mempty}
