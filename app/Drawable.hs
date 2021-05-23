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
    , darkGray
    , nextToForAxis
    , setSizeForAxis
    , emptyDrawable
    , drawableSizeForAxis
    , overlayDrawable
    , shiftInAxis
    , shiftX
    , shiftY
    , updateLayoutSizeForAxis
    ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Foreign.C.Types (CInt)

import SDL.Primitive (Color)
import qualified SDL.Primitive as GFX
import SDL.Font (Font)
import qualified SDL.Font as Font
import SDL (Point(..), V2(..), V4(..), (.+^), ($=))
import qualified SDL.Video.Renderer as Renderer
import qualified SDL

data DrawingPrimitive
    = Square Color (SDL.Rectangle CInt)
    | Ellipse Color (Point V2 CInt) (V2 CInt) -- Position and radii
    | DrawText Font Color Text (Point V2 CInt)
    | DrawTexture SDL.Texture (SDL.Rectangle CInt)

newtype LayoutData = MkLayoutData (V2 CInt) -- The widget's size
data Drawable = MkDrawable [DrawingPrimitive] LayoutData

data Axis = XAxis | YAxis

drawableSize :: Drawable -> V2 CInt
drawableSize (MkDrawable _ (MkLayoutData size)) = size

setSizeForAxis :: Axis -> CInt -> Drawable -> Drawable
setSizeForAxis axis size = updateLayoutSizeForAxis axis (const size)

updateLayoutSizeForAxis :: Axis -> (CInt -> CInt) -> Drawable -> Drawable
updateLayoutSizeForAxis XAxis f (MkDrawable visual (MkLayoutData (V2 x y))) =
    (MkDrawable visual (MkLayoutData (V2 (f x) y)))
updateLayoutSizeForAxis YAxis f (MkDrawable visual (MkLayoutData (V2 x y))) =
    (MkDrawable visual (MkLayoutData (V2 x (f y))))

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
shiftPrimitive v2 (DrawText font color text pos) = DrawText font color text (pos .+^ v2)
shiftPrimitive v2 (DrawTexture texture rect) = DrawTexture texture $ shiftRect v2 rect

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

pink, blue, lightBlue, red, black, white, darkGray :: Color
pink      = V4 255 0   255 255
blue      = V4 0   0   255 255
lightBlue = V4 33  150 243 255 -- The default blue used by Flutter
red       = V4 255 0   0   255
black     = V4 0   0   0   255
white     = V4 255 255 255 255
darkGray  = V4 100 100 100 255

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
drawPrimitive renderer (DrawText font color text pos) = do
    surface <- Font.blended font color text
    (width, height) <- Font.size font text
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.copy renderer texture Nothing $ Just $ SDL.Rectangle pos (fromIntegral <$> V2 width height)

    SDL.freeSurface surface
    Renderer.destroyTexture texture
drawPrimitive renderer (DrawTexture texture rect) = do
    SDL.copy renderer texture Nothing $ Just $ rect

draw :: SDL.Renderer -> Drawable -> IO ()
draw renderer (MkDrawable primitives _) = traverse_ (drawPrimitive renderer) primitives
