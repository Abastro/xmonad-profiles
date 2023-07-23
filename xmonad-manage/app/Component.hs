module Component (
  SetupPhase (..),
  Context (..),
  ComponentCat (..),
  Component,
  traversing,
  traversing_,
  asks,
  withIdentifier,
  withHandleWrap,
  ofHandle,
  ofAction,
  ofDependencies,
  install,
  remove,
  invoke,
  executableScripts,
) where

import Common
import Control.Applicative
import Data.Foldable
import Data.Maybe
import Manages
import Packages
import Text.Printf

data SetupPhase = Install | Remove
  deriving (Eq, Show, Enum, Bounded)
data Context mode = Custom SetupPhase | InvokeOn mode
  deriving (Eq, Show)

-- | A unit of installation. Can be conveniently merged.
--
-- When merged, former component is executed first.
--
-- Also implements Applicative and Category as well to make piping possible.
--
-- Currently, supports packages and ID specification for the component.
-- (ID is decided by the first component)
data ComponentCat mode env a = MkComponent
  { dependencies :: ![Package]
  , identifier :: !ID
  , handle :: env -> Context mode -> IO a
  }

type Component mode = ComponentCat mode ManageEnv ()

instance Show (ComponentCat mode env a) where
  show :: ComponentCat mode env a -> String
  show MkComponent{..} = printf "Component{ identifier=%s, dependencies=%s }" (show identifier) (show dependencies)

instance (Semigroup a) => Semigroup (ComponentCat mode r a) where
  (<>) :: (Semigroup a) => ComponentCat mode r a -> ComponentCat mode r a -> ComponentCat mode r a
  MkComponent dep ident han <> MkComponent dep' _ han' = MkComponent (dep <> dep') ident (han <> han')
instance (Monoid a) => Monoid (ComponentCat mode r a) where
  mempty :: (Monoid a) => ComponentCat mode r a
  mempty = MkComponent mempty (UnsafeMakeID "") mempty

instance Functor (ComponentCat mode r) where
  fmap :: (a -> b) -> ComponentCat mode r a -> ComponentCat mode r b
  fmap = liftA
instance Applicative (ComponentCat mode r) where
  pure :: a -> ComponentCat mode r a
  pure ret = MkComponent [] (UnsafeMakeID "pure") (\_ _ -> pure ret)
  (<*>) :: ComponentCat mode r (a -> b) -> ComponentCat mode r a -> ComponentCat mode r b
  f <*> x =
    MkComponent
      { dependencies = f.dependencies <> x.dependencies
      , identifier = f.identifier
      , handle = \env ctxt -> f.handle env ctxt <*> x.handle env ctxt
      }
instance Category (ComponentCat mode) where
  id :: ComponentCat mode a a
  id = MkComponent [] (UnsafeMakeID "id") (\x _ -> pure x)
  (.) :: ComponentCat mode b c -> ComponentCat mode a b -> ComponentCat mode a c
  g . f =
    MkComponent
      { dependencies = f.dependencies <> g.dependencies
      , identifier = f.identifier
      , handle = \x ctxt -> do
          y <- f.handle x ctxt
          g.handle y ctxt
      }

traversing :: (Traversable t) => ComponentCat mode a b -> ComponentCat mode (t a) (t b)
traversing transform =
  transform{handle = \col ctxt -> traverse (\x -> transform.handle x ctxt) col}

traversing_ :: (Foldable t) => ComponentCat mode a b -> ComponentCat mode (t a) ()
traversing_ transform =
  transform{handle = \col ctxt -> traverse_ (\x -> transform.handle x ctxt) col}

asks :: (Context mode -> a) -> ComponentCat mode env a
asks f =
  MkComponent
    { dependencies = []
    , identifier = UnsafeMakeID "asks"
    , handle = \_ ctxt -> pure (f ctxt)
    }

-- | Component with identifier renamed.
withIdentifier :: ID -> ComponentCat mode env a -> ComponentCat mode env a
withIdentifier identifier component = component{identifier}

withHandleWrap ::
  ((env -> Context mode -> IO a) -> (env' -> Context mode -> IO a')) ->
  ComponentCat mode env a ->
  ComponentCat mode env' a'
withHandleWrap tr component = component{handle = tr component.handle}

ofHandle :: (env -> Context mode -> IO a) -> ComponentCat mode env a
ofHandle handle = MkComponent{dependencies = [], identifier = UnsafeMakeID "handler", handle}

ofAction :: (env -> IO a) -> ComponentCat mode env a
ofAction action = ofHandle $ \env _ -> action env

ofDependencies :: [Package] -> ComponentCat mode env ()
ofDependencies dependencies = MkComponent{dependencies, identifier = UnsafeMakeID "dependencies", handle = mempty}

install :: ManageEnv -> PackageData -> InstallCond -> ComponentCat mode ManageEnv () -> IO ()
install env pkgData cond MkComponent{..} = do
  printf "Installing dependencies...\n"
  installPackages env pkgData cond dependencies
  printf "Running custom installation process...\n"
  handle env (Custom Install)

--  * When removal is being implemented, both distro and package database is needed.
remove :: env -> ComponentCat mode env () -> IO ()
remove env MkComponent{..} = do
  printf "Removing dependencies is not yet implemented.\n"
  printf "Packages %s was installed.\n" (show $ packageName <$> dependencies)
  printf "Running custom removal process...\n"
  handle env (Custom Remove)

invoke :: (Show mode) => env -> mode -> ComponentCat mode env () -> IO ()
invoke env mode MkComponent{..} = do
  printf "Invoked component in mode %s.\n" (show mode)
  handle env (InvokeOn mode)

-- | Make scripts executable, and returns the scripts.
executableScripts ::
  (Enum mode, Bounded mode) =>
  (Context mode -> Maybe FilePath) ->
  ComponentCat mode env (Maybe FilePath)
executableScripts scripts = makeExecutable *> asks scripts
  where
    makeExecutable = ofHandle $ \_ -> \case
      Custom Install -> traverse_ setToExecutable $ mapMaybe scripts allCtxt
      _ -> pure ()
    allCtxt = (Custom <$> [minBound .. maxBound]) <> (InvokeOn <$> [minBound .. maxBound])
