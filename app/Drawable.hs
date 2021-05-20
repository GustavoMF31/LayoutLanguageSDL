module Drawable
    ( Drawable(..)
    , LayoutData(..)
    , DrawingPrimitive(..)
    , draw
    , Color
    , pink
    , blue
    , lightBlue
    , red
    , black
    , white
    , nextToHorizontally
    , emptyDrawable
    , drawableSize
    , drawableWidth
    , drawableHeight
    , overlayDrawable
    , shiftX
    , shiftY
    ) where

import Data.Word (Word8)
import Data.Foldable (traverse_)
import Foreign.C.Types (CInt)

import SDL (V2(..), V4(..), (.+^), ($=))
import qualified SDL

type Color = V4 Word8

data DrawingPrimitive = Square Color (SDL.Rectangle CInt) -- | Circle Color {- radius -} CInt
newtype LayoutData = MkLayoutData (V2 CInt) -- The widget's size
data Drawable = MkDrawable [DrawingPrimitive] LayoutData

drawableSize :: Drawable -> V2 CInt
drawableSize (MkDrawable _ (MkLayoutData size)) = size

emptyDrawable :: Drawable
emptyDrawable = MkDrawable [] $ MkLayoutData $ V2 0 0

xCoord :: V2 a -> a
xCoord (V2 x _) = x

yCoord :: V2 a -> a
yCoord (V2 _ y) = y

drawableWidth :: Drawable -> CInt
drawableWidth = xCoord . drawableSize

drawableHeight :: Drawable -> CInt
drawableHeight = yCoord . drawableSize

shiftRect :: V2 CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
shiftRect v2 (SDL.Rectangle pos size) = SDL.Rectangle (pos .+^ v2) size

shiftPrimitive :: V2 CInt -> DrawingPrimitive -> DrawingPrimitive
shiftPrimitive v2 (Square color rect) = Square color $ shiftRect v2 rect

shiftPrimitives :: V2 CInt -> [DrawingPrimitive] -> [DrawingPrimitive]
shiftPrimitives v2 = map (shiftPrimitive v2)

shiftVisually :: V2 CInt -> Drawable -> Drawable
shiftVisually v2 (MkDrawable primitives layoutData) =
    MkDrawable (shiftPrimitives v2 primitives) layoutData

shiftX :: CInt -> Drawable -> Drawable
shiftX = shiftVisually . flip V2 0

shiftY :: CInt -> Drawable -> Drawable
shiftY = shiftVisually . V2 0

overlayDrawable :: Drawable -> Drawable -> Drawable
overlayDrawable = combineDrawable $ V2 max max

nextToHorizontally :: Drawable -> Drawable -> Drawable
nextToHorizontally = combineDrawable $ V2 (+) max

combineDrawable
    :: V2 (CInt -> CInt -> CInt)
    -> Drawable -> Drawable -> Drawable
combineDrawable combineSizes
    (MkDrawable primitivesBelow (MkLayoutData size))
    (MkDrawable primitivesAbove (MkLayoutData size')) =
      MkDrawable (primitivesBelow ++ primitivesAbove) (MkLayoutData $ combineSizes <*> size <*> size')

pink, blue, lightBlue, red, black, white :: Color
pink      = V4 255 0   255 255
blue      = V4 0   0   255 255
lightBlue = V4 33  150 243 255 -- The default blue used by Flutter
red       = V4 255 0   0   255
black     = V4 0   0   0   255
white     = V4 255 255 255 255

drawPrimitive :: SDL.Renderer -> DrawingPrimitive -> IO ()
drawPrimitive renderer (Square color rectangle) = do
    SDL.rendererDrawColor renderer $= color
    SDL.fillRect renderer (Just rectangle)

draw :: SDL.Renderer -> Drawable -> IO ()
draw renderer (MkDrawable primitives _) = traverse_ (drawPrimitive renderer) primitives
