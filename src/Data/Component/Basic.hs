{-| The package provides the basics for all components in the library.

A component should have the followings (those can be omitted):

- position: @{x: int, y: int}@
- size: @{width: int, height: int}@
- color: @int[4]@
- font: @{family: string, bold: bool, italic: bool, size: int}@

-}
module Data.Component.Basic where

import Data.Aeson
import Data.Word (Word8)
import qualified SDL.Vect as Vect
import qualified SDL.Font
import MiniLight

data Config = Config {
  size :: Vect.V2 Int,
  position :: Vect.V2 Int,
  color :: Vect.V4 Word8,
  fontDesc :: FontDescriptor,
  fontSize :: Int
}

instance FromJSON Config where
  parseJSON = withObject "config" $ \v -> do
    sizeMaybe <- v .:? "size"
    size <- (\w -> maybe (return 0) w sizeMaybe) $ withObject "size" $ \v ->
      Vect.V2 <$> v .: "width" <*> v .: "height"

    positionMaybe <- v .:? "position"
    position <- (\w -> maybe (return 0) w positionMaybe) $ withObject "position" $ \v ->
      Vect.V2 <$> v .: "x" <*> v .: "y"

    color <- fmap (\[r,g,b,a] -> Vect.V4 r g b a) $ v .:? "color" .!= [255, 255, 255, 255]

    fontMaybe <- v .:? "font"
    (fontDesc, fontSize) <- (\w -> maybe (return (FontDescriptor "" (FontStyle False False), 0)) w fontMaybe) $ withObject "font" $ \v -> do
      family <- v .: "family"
      size <- v .: "size"
      bold <- v .:? "bold" .!= False
      italic <- v .:? "italic" .!= False

      return $ (FontDescriptor family (FontStyle bold italic), size)

    return $ Config size position color fontDesc fontSize

loadFontFrom :: Config -> MiniLight SDL.Font.Font
loadFontFrom conf = loadFont (fontDesc conf) (fontSize conf)
