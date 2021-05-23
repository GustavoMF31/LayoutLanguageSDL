{-# LANGUAGE DataKinds, TypeFamilyDependencies, NoStarIsType, TupleSections #-}

module Widget
    ( WidgetSizeDependency(..)
    , Widget
    , empty
    , row
    , column
    , before
    , after
    , atop
    , above
    , flexibleSquare
    , square
    , flexibleCircle
    , limitSizeX
    , limitSizeY
    , limitSize
    , alignRatioX
    , alignRatioY
    , alignRatio
    , alignLeft
    , centerX
    , alignRight
    , alignTop
    , centerY
    , alignBottom
    , center
    , distributedX
    , spaceEvenlyX
    , overlay
    , coloredBackgroud
    , aspectRatio
    , marginTop
    , marginRight
    , marginDown
    , marginLeft
    , marginEach
    , marginAround
    , constMarginTop
    , constMarginRight
    , constMarginDown
    , constMarginLeft
    , constMarginEach
    , constMarginAround
    , matchHeightBelow
    , stack
    , text
    , flexibleImage
    , image
    , beneath
    ) where

import Data.Kind (Type)
import Data.Text (Text)

import Foreign.C.Types (CInt)
import System.IO.Unsafe

import SDL (V2(..), Point(..))
import qualified SDL (Rectangle(..), Texture)
import SDL.Font (Font)
import qualified SDL.Font as Font

import Drawable
    ( Drawable(..)
    , LayoutData(..)
    , DrawingPrimitive(..)
    , Axis(..)
    , Color
    , nextToForAxis
    , setSizeForAxis
    , overlayDrawable
    , emptyDrawable
    , shiftInAxis
    , drawableSizeForAxis
    , updateLayoutSizeForAxis
    )

data WidgetSizeDependency
    = Constant
    | Flexible

-- type WidgetDependency :: WidgetSizeDependency -> Type
type family WidgetDependency (x :: WidgetSizeDependency) = (r :: Type) | r -> x where
    WidgetDependency Constant = ()
    WidgetDependency Flexible = CInt

-- Widget :: WidgetSizeDependency -> WidgetSizeDependency -> Type
type Widget a b = WidgetDependency a -> WidgetDependency b -> Drawable

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- Subtracts two `CInt`s, but ensures the result is not negative
subtractPixels :: CInt -> CInt -> CInt
subtractPixels = max 0 .: (-)

flexibleSquare :: Color -> Widget Flexible Flexible
flexibleSquare color width height =
    let size = V2 width height
    in MkDrawable [Square color (SDL.Rectangle (P $ V2 0 0) size)] $ MkLayoutData size

square :: CInt -> CInt -> Color -> Widget Constant Constant
square x y = limitSize x y . flexibleSquare

-- Since the circle is flexible, it will often look like an ellipse instead
flexibleCircle :: Color -> Widget Flexible Flexible
flexibleCircle color diameterX diameterY =
    let diameters = V2 diameterX diameterY
        radii = div <$> diameters <*> pure 2
    in MkDrawable [Ellipse color (P $ V2 0 0) radii]  $ MkLayoutData diameters

besideForAxis :: Axis -> Widget Constant a -> Widget Constant a -> Widget Constant a
besideForAxis axis f g _ yConstraint  = nextToForAxis axis fDrawable $ shiftInAxis axis (drawableSizeForAxis axis fDrawable) gDrawable
  where
    fDrawable, gDrawable :: Drawable
    fDrawable = f () yConstraint
    gDrawable = g () yConstraint

beside :: Widget Constant a -> Widget Constant a -> Widget Constant a
beside = besideForAxis XAxis

adjustForYAxis
    :: ((b' -> a' -> c') -> b -> a -> c)
    ->  (a' -> b' -> c') -> a -> b -> c
adjustForYAxis f = flip . f . flip

adjustForYAxis2
    :: ((b' -> a' -> c') -> (b'' -> a'' -> c'') -> b -> a -> c)
    ->  (a' -> b' -> c') -> (a'' -> b'' -> c'') -> a -> b -> c
adjustForYAxis2 f a b = flip $ f (flip a) (flip b)

vertically :: Widget a Constant -> Widget a Constant -> Widget a Constant
vertically = adjustForYAxis2 $ besideForAxis YAxis

adjacentForAxis :: Axis -> Widget Constant a -> Widget Flexible a -> Widget Flexible a
adjacentForAxis axis f g xConstraint yConstraint = nextToForAxis axis fDrawable $
    shiftInAxis axis fSize $ g (subtractPixels xConstraint fSize) yConstraint
  where
    fDrawable :: Drawable
    fDrawable = f () yConstraint

    fSize :: CInt
    fSize = drawableSizeForAxis axis fDrawable

reverseAdjacentForAxis :: Axis -> Widget Flexible a -> Widget Constant a -> Widget Flexible a
reverseAdjacentForAxis axis f g xConstraint yConstraint = nextToForAxis axis
    fDrawable
    (shiftInAxis axis fSize gDrawable)
  where
    gSize :: CInt
    gSize = drawableSizeForAxis axis $ gDrawable

    fSize :: CInt
    fSize = drawableSizeForAxis axis $ fDrawable

    gDrawable :: Drawable
    gDrawable = g () yConstraint

    fDrawable :: Drawable
    fDrawable = f (subtractPixels xConstraint gSize) yConstraint

after :: Widget Flexible a -> Widget Constant a -> Widget Flexible a
after = reverseAdjacentForAxis XAxis

-- This name only makes sense infix
before :: Widget Constant a -> Widget Flexible a -> Widget Flexible a
before = adjacentForAxis XAxis

above :: Widget a Flexible -> Widget a Constant -> Widget a Flexible
above = adjustForYAxis2 $ reverseAdjacentForAxis YAxis

atop :: Widget a Constant -> Widget a Flexible -> Widget a Flexible
atop = adjustForYAxis2 $ adjacentForAxis YAxis

alignRatioForAxis
    :: Axis
    -> (WidgetDependency a -> Drawable)
    -> Double
    -> Widget Flexible a
alignRatioForAxis axis getDrawable ratio availableSize otherConstraint =
    let drawable = getDrawable otherConstraint
    in setSizeForAxis axis availableSize $ shiftInAxis axis (round $ ratio * fromIntegral (availableSize - drawableSizeForAxis axis drawable)) drawable

alignRatioX :: Double -> Widget Constant a -> Widget Flexible a
alignRatioX r w = alignRatioForAxis XAxis (w ()) r

alignRatioY :: Double -> Widget a Constant -> Widget a Flexible
alignRatioY r w = flip $ alignRatioForAxis YAxis (flip w ()) r

empty :: Widget a b
empty _ _ = emptyDrawable

{-
spacerX :: CInt -> Widget Constant a
spacerX width _ _ = MkDrawable [] (V2 0 width)

spacerY :: CInt -> Widget a Constant
spacerY height _ _ = MkDrawable [] (V2 height 0)

spacer :: CInt -> Widget Constant Constant
spacer width height _ _ = MkDrawable [] (V2 Widget height)
-}

row :: [Widget Constant a] -> Widget Constant a
row = foldr beside empty

column :: [Widget a Constant] -> Widget a Constant
column = foldr vertically empty

limitSizeX :: CInt -> Widget Flexible a -> Widget Constant a
limitSizeX size f _ = f size

limitSizeY :: CInt -> Widget a Flexible -> Widget a Constant
limitSizeY size f = flip $ const $ flip f size

limitSize :: CInt -> CInt -> Widget Flexible Flexible -> Widget Constant Constant
limitSize limitX limitY = limitSizeY limitY . limitSizeX limitX

alignRatio :: Double -> Double -> Widget Constant Constant -> Widget Flexible Flexible
alignRatio ratioX ratioY = alignRatioX ratioX . alignRatioY ratioY

alignLeft :: Widget Constant a -> Widget Flexible a
alignLeft = alignRatioX 0

centerX :: Widget Constant a -> Widget Flexible a
centerX = alignRatioX 0.5

alignRight :: Widget Constant a -> Widget Flexible a
alignRight = alignRatioX 1

alignTop :: Widget a Constant -> Widget a Flexible
alignTop = alignRatioY 0

centerY :: Widget a Constant -> Widget a Flexible
centerY = alignRatioY 0.5

alignBottom :: Widget a Constant -> Widget a Flexible
alignBottom = alignRatioY 1

center :: Widget Constant Constant -> Widget Flexible Flexible
center = centerX . centerY

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just $ div x y

distributedX :: [(CInt, Widget Flexible a)] -> Widget Flexible a
distributedX widgets availableWidth = case safeDiv availableWidth (sum $ map fst widgets) of
    Nothing -> const emptyDrawable
    Just widthPerPart -> ($ ()) $ row $ map (\(parts, widget) () -> widget (parts * widthPerPart)) widgets

spaceEvenlyX :: [Widget Flexible a] -> Widget Flexible a
spaceEvenlyX = distributedX . map (1,)

overlay :: Widget a b -> Widget a b -> Widget a b
overlay f g constraintX constraintY = overlayDrawable (f constraintX constraintY) (g constraintX constraintY)

coloredBackgroud :: Color -> Widget Flexible Flexible -> Widget Flexible Flexible
coloredBackgroud = overlay . flexibleSquare

aspectRatio :: Double -> Widget Flexible Flexible -> Widget Flexible Flexible
aspectRatio goalRatio w xConstraint yConstraint
    | actualRatio > goalRatio = w (round $ goalRatio * fromIntegral yConstraint) yConstraint
    | otherwise = w xConstraint (round $ goalRatio * fromIntegral xConstraint)
  where
    actualRatio = fromIntegral xConstraint / fromIntegral yConstraint

marginForAxis :: (CInt -> CInt) -> Axis -> CInt -> Widget Flexible a -> Widget Flexible a
marginForAxis determineShift axis margin w maxX constraintY = setSizeForAxis axis maxX $
    shiftInAxis axis (determineShift margin) $
    w (subtractPixels maxX margin) constraintY

marginLeft :: CInt -> Widget Flexible a -> Widget Flexible a
marginLeft = marginForAxis id XAxis

marginTop :: CInt -> Widget a Flexible -> Widget a Flexible
marginTop = adjustForYAxis . marginForAxis id YAxis

marginRight :: CInt -> Widget Flexible a -> Widget Flexible a
marginRight = marginForAxis (const 0) XAxis

marginDown :: CInt -> Widget a Flexible -> Widget a Flexible
marginDown = adjustForYAxis . marginForAxis (const 0) YAxis

marginEach :: CInt -> CInt -> CInt -> CInt -> Widget Flexible Flexible -> Widget Flexible Flexible
marginEach top right down left = marginTop top . marginDown down . marginLeft left . marginRight right

marginAround :: CInt -> Widget Flexible Flexible -> Widget Flexible Flexible
marginAround x = marginEach x x x x

constMarginForAxis :: Axis -> (CInt -> CInt) -> CInt -> Widget Constant a -> Widget Constant a
constMarginForAxis axis determineShift marginSize w () yConstraint = shiftInAxis axis (determineShift marginSize) $
    updateLayoutSizeForAxis axis (+ marginSize) (w () yConstraint)

constMarginLeft :: CInt -> Widget Constant a -> Widget Constant a
constMarginLeft = constMarginForAxis XAxis id

constMarginRight :: CInt -> Widget Constant a -> Widget Constant a
constMarginRight = constMarginForAxis XAxis (const 0)

constMarginTop :: CInt -> Widget a Constant -> Widget a Constant
constMarginTop = adjustForYAxis . constMarginForAxis YAxis id

constMarginDown :: CInt -> Widget a Constant -> Widget a Constant
constMarginDown = adjustForYAxis . constMarginForAxis YAxis (const 0)

constMarginEach :: CInt -> CInt -> CInt -> CInt -> Widget Constant Constant -> Widget Constant Constant
constMarginEach top right down left =
      constMarginTop top . constMarginRight right
    . constMarginDown down . constMarginLeft left

constMarginAround :: CInt -> Widget Constant Constant -> Widget Constant Constant
constMarginAround x = constMarginEach x x x x

beneath :: Widget Constant Constant -> Widget Flexible Flexible -> Widget Constant Constant
beneath constant flexible = overlay (limitSize (drawableSizeForAxis XAxis drawable) (drawableSizeForAxis YAxis drawable) flexible) constant
  where
    drawable :: Drawable
    drawable = constant () ()

matchHeightBelow :: Widget a Constant -> Widget a Flexible -> Widget a Constant
matchHeightBelow constant flexible xConstraint () = overlayDrawable
    ((\x -> x xConstraint ()) $ limitSizeY (drawableSizeForAxis YAxis drawable) $ flexible)
    drawable
  where
    drawable = constant xConstraint ()

stack :: [Widget a b] -> Widget a b
stack = foldr overlay empty

pairToV2 :: (a, a) -> V2 a
pairToV2 (x, y) = V2 x y

-- Getting the size of text does not perform actual side effects, so, unsafePerformIO should not cause trouble
-- Reworking the system to call this function in IO would be a significant pain
textSizeForFont :: Font -> Text -> V2 CInt
textSizeForFont font t = fmap fromIntegral $ pairToV2 $ System.IO.Unsafe.unsafePerformIO $ Font.size font t

text :: Font -> Color -> Text -> Widget Constant Constant
text font color t () () = MkDrawable [DrawText font color t (P $ V2 0 0)] (MkLayoutData $ textSizeForFont font t)

flexibleImage :: SDL.Texture -> Widget Flexible Flexible
flexibleImage texture xSize ySize =
    let size = V2 xSize ySize
    in MkDrawable [DrawTexture texture (SDL.Rectangle (P $ V2 0 0) size)] (MkLayoutData size)

image :: CInt -> CInt -> SDL.Texture -> Widget Constant Constant
image x y = limitSize x y . flexibleImage
