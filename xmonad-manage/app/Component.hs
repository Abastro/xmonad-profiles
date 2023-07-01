module Component where

import Packages
import Manages

data Context a = CustomInstall | CustomRemove | Invoke a

-- | A unit of installation. Can be conveniently merged.
data Component a = MkComponent
  { dependencies :: [Package]
  , handle :: ManageEnv -> Context a -> IO ()
  }
