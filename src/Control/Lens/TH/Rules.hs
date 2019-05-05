module Control.Lens.TH.Rules where

import Control.Lens
import Language.Haskell.TH

-- | Custom rules of the lens. All lens are prefixed by '_'.
lensRules_ :: LensRules
lensRules_ =
  lensRules & lensField .~ \_ _ n -> [TopName (mkName ('_' : nameBase n))]
