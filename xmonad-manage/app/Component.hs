module Component (
  SetupPhase (..),
  Context (..),
  ComponentCat (..),
  Component,
  traversing,
  between,
  install,
  remove,
  invoke,
  fromScript,
  ofHandle,
  ofAction,
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
--
-- Also implements Applicative and Category as well to make piping possible.
data ComponentCat mode env a = MkComponent
  { dependencies :: [Package]
  , handle :: env -> Context mode -> IO a
  }

type Component mode = ComponentCat mode ManageEnv ()

instance (Semigroup a) => Semigroup (ComponentCat mode r a) where
  (<>) :: (Semigroup a) => ComponentCat mode r a -> ComponentCat mode r a -> ComponentCat mode r a
  MkComponent dep han <> MkComponent dep' han' = MkComponent (dep <> dep') (han <> han')
instance (Monoid a) => Monoid (ComponentCat mode r a) where
  mempty :: (Monoid a) => ComponentCat mode r a
  mempty = MkComponent mempty mempty

instance Functor (ComponentCat mode r) where
  fmap :: (a -> b) -> ComponentCat mode r a -> ComponentCat mode r b
  fmap = liftA
instance Applicative (ComponentCat mode r) where
  pure :: a -> ComponentCat mode r a
  pure ret = MkComponent [] (\_ _ -> pure ret)
  (<*>) :: ComponentCat mode r (a -> b) -> ComponentCat mode r a -> ComponentCat mode r b
  f <*> x =
    MkComponent
      { dependencies = f.dependencies <> x.dependencies
      , handle = \env ctxt -> f.handle env ctxt <*> x.handle env ctxt
      }
instance Category (ComponentCat mode) where
  id :: ComponentCat mode a a
  id = MkComponent [] (\x _ -> pure x)
  (.) :: ComponentCat mode b c -> ComponentCat mode a b -> ComponentCat mode a c
  g . f =
    MkComponent
      { dependencies = f.dependencies <> g.dependencies
      , handle = \x ctxt -> do
          y <- f.handle x ctxt
          g.handle y ctxt
      }

traversing :: (Traversable t) => ComponentCat mode a b -> ComponentCat mode (t a) (t b)
traversing transform =
  transform{handle = \col ctxt -> traverse (\x -> transform.handle x ctxt) col}

between :: ComponentCat mode env a -> ComponentCat mode a b -> ComponentCat mode a c -> ComponentCat mode env c
between pre post inside = pre >>> (inside <* post)

install :: ManageEnv -> PkgDatabase -> ManageID -> InstallCond -> ComponentCat mode ManageEnv () -> IO ()
install env pkgDb distro cond MkComponent{..} = do
  printf "Installing dependencies...\n"
  installPackages env pkgDb distro cond dependencies
  printf "Running custom installation process...\n"
  handle env (Custom Install)

--  * When removal is being implemented, both distro and package database is needed.
remove :: env -> ComponentCat mode env () -> IO ()
remove env MkComponent{..} = do
  printf "Removing dependencies is not yet implemented.\n"
  printf "Packages %s was installed.\n" (show $ packageName <$> dependencies)
  printf "Running custom removal process...\n"
  handle env (Custom Remove)

invoke :: env -> mode -> ComponentCat mode env () -> IO ()
invoke env mode MkComponent{..} = do
  handle env (InvokeOn mode)

-- | Make a component from script, which uses given script executor.
fromScript ::
  (Enum mode, Bounded mode) =>
  (env -> FilePath -> Context mode -> IO ()) ->
  (SetupPhase -> Maybe FilePath) ->
  (mode -> FilePath) ->
  ComponentCat mode env ()
fromScript executor setupScripts invokeScripts = withSetupScripts <> withInvokeScripts
  where
    withSetupScripts = ofHandle $ \env -> \case
      -- Needs to be executable
      Custom phase -> for_ (setupScripts phase) $ \setup -> do
        setToExecutable setup
        executor env setup (Custom phase)
      InvokeOn _ -> pure ()
    withInvokeScripts = ofHandle $ \env -> \case
      -- Needs to be executable
      Custom Install -> for_ [minBound .. maxBound] $ \mode -> setToExecutable (invokeScripts mode)
      Custom Remove -> pure ()
      InvokeOn mode -> executor env (invokeScripts mode) (InvokeOn mode)

ofHandle :: (env -> Context mode -> IO a) -> ComponentCat mode env a
ofHandle handle = MkComponent{dependencies = [], handle}

ofAction :: (env -> IO a) -> ComponentCat mode env a
ofAction action = ofHandle $ \env _ -> action env

ofDependencies :: [Package] -> ComponentCat mode env ()
ofDependencies dependencies = MkComponent{dependencies, handle = mempty}
