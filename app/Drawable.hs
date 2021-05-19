module Drawable
    ( Drawable(..)
    , LayoutData(..)
    , DrawingPrimitive(..)
    , draw
    , Color
    , pink
    , blue
    , red
    , black
    , drawableSize
    , drawablePosition
    , shiftPrimitives
    , shiftBy
    , shiftX
    , shiftY
    , xCoord
    ) where

import Data.Word (Word8)
import Data.Foldable (traverse_)
import Foreign.C.Types (CInt)

import SDL (V2(..), V4(..), Point, (.+^), ($=))
import qualified SDL

type Color = V4 Word8

data DrawingPrimitive = Square Color (SDL.Rectangle CInt) -- | Circle Color {- radius -} CInt
newtype LayoutData = MkLayoutData (SDL.Rectangle CInt) -- The widget's bounding box
data Drawable = MkDrawable [DrawingPrimitive] LayoutData

drawableSize :: Drawable -> V2 CInt
drawableSize (MkDrawable _ (MkLayoutData (SDL.Rectangle _ size))) = size

drawablePosition :: Drawable -> Point V2 CInt
drawablePosition (MkDrawable _ (MkLayoutData (SDL.Rectangle pos _))) = pos

xCoord :: V2 a -> a
xCoord (V2 x _) = x

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

pink :: Color
pink = V4 255 0 255 255

blue :: Color
blue = V4 0 0 255 255

red :: Color
red = V4 255 0 0 255

black :: Color
black = V4 0 0 0 255

drawPrimitive :: SDL.Renderer -> DrawingPrimitive -> IO ()
drawPrimitive renderer (Square color rectangle) = do
    SDL.rendererDrawColor renderer $= color
    SDL.fillRect renderer (Just rectangle)

draw :: SDL.Renderer -> Drawable -> IO ()
draw renderer (MkDrawable primitives _) = traverse_ (drawPrimitive renderer) primitives
