module Drawable
    ( Drawable(..)
    , LayoutData(..)
    , DrawingPrimitive(..)
    , Axis(..)
    , draw
    , Color
    , pink
    , blue
    , lightBlue
    , red
    , black
    , white
    , nextToForAxis
    , setSizeForAxis
    , emptyDrawable
    , drawableSizeForAxis
    , overlayDrawable
    , shiftInAxis
    , shiftX
    , shiftY
    ) where

import Data.Foldable (traverse_)
import Foreign.C.Types (CInt)

import SDL (Point(..), V2(..), V4(..), (.+^), ($=))
import SDL.Primitive (Color)
import qualified SDL.Primitive as GFX
import qualified SDL

data DrawingPrimitive
    = Square Color (SDL.Rectangle CInt)
    | Ellipse Color (Point V2 CInt) (V2 CInt) -- Position and radii

newtype LayoutData = MkLayoutData (V2 CInt) -- The widget's size
data Drawable = MkDrawable [DrawingPrimitive] LayoutData


data Axis = XAxis | YAxis

drawableSize :: Drawable -> V2 CInt
drawableSize (MkDrawable _ (MkLayoutData size)) = size

setSizeForAxis :: Axis -> CInt -> Drawable -> Drawable
setSizeForAxis XAxis size (MkDrawable visual (MkLayoutData (V2 _ y))) =
    (MkDrawable visual (MkLayoutData (V2 size y)))
setSizeForAxis YAxis size (MkDrawable visual (MkLayoutData (V2 x _))) =
    (MkDrawable visual (MkLayoutData (V2 x size)))

emptyDrawable :: Drawable
emptyDrawable = MkDrawable [] $ MkLayoutData $ V2 0 0

getComponent :: Axis -> V2 a -> a
getComponent XAxis (V2 x _) =  x
getComponent YAxis (V2 _ y) =  y

drawableSizeForAxis :: Axis -> Drawable -> CInt
drawableSizeForAxis axis = getComponent axis . drawableSize

shiftRect :: V2 CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
shiftRect v2 (SDL.Rectangle pos size) = SDL.Rectangle (pos .+^ v2) size

shiftPrimitive :: V2 CInt -> DrawingPrimitive -> DrawingPrimitive
shiftPrimitive v2 (Square color rect) = Square color $ shiftRect v2 rect
shiftPrimitive v2 (Ellipse color pos radii) = Ellipse color (pos .+^ v2) radii

shiftPrimitives :: V2 CInt -> [DrawingPrimitive] -> [DrawingPrimitive]
shiftPrimitives v2 = map (shiftPrimitive v2)

shiftVisually :: V2 CInt -> Drawable -> Drawable
shiftVisually v2 (MkDrawable primitives layoutData) =
    MkDrawable (shiftPrimitives v2 primitives) layoutData

shiftX :: CInt -> Drawable -> Drawable
shiftX = shiftVisually . flip V2 0

shiftY :: CInt -> Drawable -> Drawable
shiftY = shiftVisually . V2 0

shiftInAxis :: Axis -> CInt -> Drawable -> Drawable
shiftInAxis XAxis = shiftX
shiftInAxis YAxis = shiftY

overlayDrawable :: Drawable -> Drawable -> Drawable
overlayDrawable = combineDrawable $ V2 max max

nextToHorizontally :: Drawable -> Drawable -> Drawable
nextToHorizontally = combineDrawable $ V2 (+) max

nextToVertically :: Drawable -> Drawable -> Drawable
nextToVertically = combineDrawable $ V2 max (+)

nextToForAxis :: Axis -> Drawable -> Drawable -> Drawable
nextToForAxis XAxis = nextToHorizontally
nextToForAxis YAxis = nextToVertically

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
drawPrimitive renderer (Ellipse color (P pos) radii@(V2 xRadius yRadius)) = do
    GFX.fillEllipse renderer (pos + radii) xRadius yRadius color

    -- Rendering these extra outlines is a hack to make the resulting circle
    -- a bit smoother
    GFX.smoothEllipse renderer (pos + radii) (xRadius - 1) (yRadius - 1) color
    GFX.smoothEllipse renderer (pos + radii) xRadius yRadius color

draw :: SDL.Renderer -> Drawable -> IO ()
draw renderer (MkDrawable primitives _) = traverse_ (drawPrimitive renderer) primitives
