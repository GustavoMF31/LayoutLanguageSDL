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
    , combineRect
    , nextToHorizontally
    , emptyDrawable
    , drawableSize
    , drawablePosition
    , drawableWidth
    , drawableHeight
    , overlayDrawable
    , shiftPrimitives
    , shiftBy
    , shiftX
    , shiftY
    , xCoord
    ) where

import Data.Word (Word8)
import Data.Foldable (traverse_)
import Foreign.C.Types (CInt)

import SDL (V2(..), V4(..), Point(..), (.+^), ($=))
import qualified SDL

type Color = V4 Word8

data DrawingPrimitive = Square Color (SDL.Rectangle CInt) -- | Circle Color {- radius -} CInt
-- TODO: Make the layout data hold just the size, without an accompanying position
newtype LayoutData = MkLayoutData (SDL.Rectangle CInt) -- The widget's bounding box
data Drawable = MkDrawable [DrawingPrimitive] LayoutData

drawableSize :: Drawable -> V2 CInt
drawableSize (MkDrawable _ (MkLayoutData (SDL.Rectangle _ size))) = size

drawablePosition :: Drawable -> Point V2 CInt
drawablePosition (MkDrawable _ (MkLayoutData (SDL.Rectangle pos _))) = pos

emptyDrawable :: Drawable
emptyDrawable = MkDrawable [] $ MkLayoutData $ SDL.Rectangle (P $ V2 0 0) (V2 0 0)

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

shiftPhysically :: V2 CInt -> Drawable -> Drawable
shiftPhysically v2 (MkDrawable primitives (MkLayoutData rect)) =
    MkDrawable primitives $ MkLayoutData $ shiftRect v2 rect

shiftBy :: V2 CInt -> Drawable -> Drawable
shiftBy v2 = shiftPhysically v2 . shiftVisually v2

shiftX :: CInt -> Drawable -> Drawable
shiftX = shiftBy . flip V2 0

shiftY :: CInt -> Drawable -> Drawable
shiftY = shiftBy . V2 0

combineRect
    :: (CInt -> CInt -> CInt)
    -> (CInt -> CInt -> CInt)
    -> (CInt -> CInt -> CInt)
    -> (CInt -> CInt -> CInt)
    -> SDL.Rectangle CInt
    -> SDL.Rectangle CInt
    -> SDL.Rectangle CInt
combineRect f g j k (SDL.Rectangle (P (V2 x y)) (V2 w h)) (SDL.Rectangle (P (V2 x' y')) (V2 w' h')) =
    SDL.Rectangle (P $ V2 (f x x') (g y y')) (V2 (j w w') (k h h'))

overlayDrawable :: Drawable -> Drawable -> Drawable
overlayDrawable = combineDrawable $ combineRect min min max max

nextToHorizontally :: Drawable -> Drawable -> Drawable
nextToHorizontally = combineDrawable $ combineRect min min (+) max

combineDrawable
    :: (SDL.Rectangle CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt)
    -> Drawable -> Drawable -> Drawable
combineDrawable f (MkDrawable primitivesBelow (MkLayoutData rect)) (MkDrawable primitivesAbove (MkLayoutData rect')) =
    MkDrawable (primitivesBelow ++ primitivesAbove) (MkLayoutData $ f rect rect')

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
