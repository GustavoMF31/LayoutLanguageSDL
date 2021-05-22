{-# LANGUAGE DataKinds, TypeFamilyDependencies, NoStarIsType, TupleSections #-}

module Widget
    ( WidgetSizeDependency(..)
    , Widget
    , beside
    , row
    , below
    , column
    , toTheLeftOf
    , atop
    , flexibleSquare
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
    , text
    , flexibleImage
    , image
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
    )

data WidgetSizeDependency
    = ConstantSized
    | MaxBounded
    | MinMaxBounded

-- type WidgetDependency :: WidgetSizeDependency -> Type
type family WidgetDependency (x :: WidgetSizeDependency) = (r :: Type) | r -> x where
    WidgetDependency ConstantSized = ()
    WidgetDependency MaxBounded = CInt
    WidgetDependency MinMaxBounded = (CInt, CInt)

-- Widget :: WidgetSizeDependency -> WidgetSizeDependency -> Type
type Widget a b = WidgetDependency a -> WidgetDependency b -> Drawable

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- Subtracts two `CInt`s, but ensures the result is not negative
subtractPixels :: CInt -> CInt -> CInt
subtractPixels = max 0 .: (-)

flexibleSquare :: Color -> Widget MaxBounded MaxBounded
flexibleSquare color width height =
    let size = V2 width height
    in MkDrawable [Square color (SDL.Rectangle (P $ V2 0 0) size)] $ MkLayoutData size

-- Since the circle is flexible, it will often look like an ellipse instead
flexibleCircle :: Color -> Widget MaxBounded MaxBounded
flexibleCircle color diameterX diameterY =
    let diameters = V2 diameterX diameterY
        radii = div <$> diameters <*> pure 2
    in MkDrawable [Ellipse color (P $ V2 0 0) radii]  $ MkLayoutData diameters

besideForAxis :: Axis -> Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
besideForAxis axis f g _ yConstraint  = nextToForAxis axis fDrawable $ shiftInAxis axis (drawableSizeForAxis axis fDrawable) gDrawable
  where
    fDrawable, gDrawable :: Drawable
    fDrawable = f () yConstraint
    gDrawable = g () yConstraint

beside :: Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
beside = besideForAxis XAxis

adjustForYAxis
    :: ((b' -> a' -> c') -> b -> a -> c)
    ->  (a' -> b' -> c') -> a -> b -> c
adjustForYAxis f = flip . f . flip

adjustForYAxis2
    :: ((b' -> a' -> c') -> (b'' -> a'' -> c'') -> b -> a -> c)
    ->  (a' -> b' -> c') -> (a'' -> b'' -> c'') -> a -> b -> c
adjustForYAxis2 f a b = flip $ f (flip a) (flip b)

below :: Widget a ConstantSized -> Widget a ConstantSized -> Widget a ConstantSized
below = adjustForYAxis2 $ besideForAxis YAxis

adjacentForAxis :: Axis -> Widget ConstantSized a -> Widget MaxBounded a -> Widget MaxBounded a
adjacentForAxis axis f g xConstraint yConstraint = nextToForAxis axis fDrawable $
    shiftInAxis axis fSize $ g (subtractPixels xConstraint fSize) yConstraint
  where
    fDrawable :: Drawable
    fDrawable = f () yConstraint

    fSize :: CInt
    fSize = drawableSizeForAxis axis fDrawable

toTheLeftOf :: Widget ConstantSized a -> Widget MaxBounded a -> Widget MaxBounded a
toTheLeftOf = adjacentForAxis XAxis

atop :: Widget a ConstantSized -> Widget a MaxBounded -> Widget a MaxBounded
atop = adjustForYAxis2 $ adjacentForAxis YAxis

alignRatioForAxis
    :: Axis
    -> (WidgetDependency a -> Drawable)
    -> Double
    -> Widget MaxBounded a
alignRatioForAxis axis getDrawable ratio availableSize otherConstraint =
    let drawable = getDrawable otherConstraint
    in setSizeForAxis axis availableSize $ shiftInAxis axis (round $ ratio * fromIntegral (availableSize - drawableSizeForAxis axis drawable)) drawable

alignRatioX :: Double -> Widget ConstantSized a -> Widget MaxBounded a
alignRatioX r w = alignRatioForAxis XAxis (w ()) r

alignRatioY :: Double -> Widget a ConstantSized -> Widget a MaxBounded
alignRatioY r w = flip $ alignRatioForAxis YAxis (flip w ()) r

empty :: Widget a b
empty _ _ = emptyDrawable

{-
spacerX :: CInt -> Widget ConstantSized a
spacerX width _ _ = MkDrawable [] (V2 0 width)

spacerY :: CInt -> Widget a ConstantSized
spacerY height _ _ = MkDrawable [] (V2 height 0)

spacer :: CInt -> Widget ConstantSized ConstantSized
spacer width height _ _ = MkDrawable [] (V2 Widget height)
-}

row :: [Widget ConstantSized a] -> Widget ConstantSized a
row = foldr beside empty

column :: [Widget a ConstantSized] -> Widget a ConstantSized
column = foldr below empty

limitSizeX :: CInt -> Widget MaxBounded a -> Widget ConstantSized a
limitSizeX size f _ = f size

limitSizeY :: CInt -> Widget a MaxBounded -> Widget a ConstantSized
limitSizeY size f = flip $ const $ flip f size

limitSize :: CInt -> CInt -> Widget MaxBounded MaxBounded -> Widget ConstantSized ConstantSized
limitSize limitX limitY = limitSizeY limitY . limitSizeX limitX

alignRatio :: Double -> Double -> Widget ConstantSized ConstantSized -> Widget MaxBounded MaxBounded
alignRatio ratioX ratioY = alignRatioX ratioX . alignRatioY ratioY

alignLeft :: Widget ConstantSized a -> Widget MaxBounded a
alignLeft = alignRatioX 0

centerX :: Widget ConstantSized a -> Widget MaxBounded a
centerX = alignRatioX 0.5

alignRight :: Widget ConstantSized a -> Widget MaxBounded a
alignRight = alignRatioX 1

alignTop :: Widget a ConstantSized -> Widget a MaxBounded
alignTop = alignRatioY 0

centerY :: Widget a ConstantSized -> Widget a MaxBounded
centerY = alignRatioY 0.5

alignBottom :: Widget a ConstantSized -> Widget a MaxBounded
alignBottom = alignRatioY 1

center :: Widget ConstantSized ConstantSized -> Widget MaxBounded MaxBounded
center = centerX . centerY

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just $ div x y

distributedX :: [(CInt, Widget MaxBounded a)] -> Widget MaxBounded a
distributedX widgets availableWidth = case safeDiv availableWidth (sum $ map fst widgets) of
    Nothing -> const emptyDrawable
    Just widthPerPart -> ($ ()) $ row $ map (\(parts, widget) () -> widget (parts * widthPerPart)) widgets

spaceEvenlyX :: [Widget MaxBounded a] -> Widget MaxBounded a
spaceEvenlyX = distributedX . map (1,)

overlay :: Widget a b -> Widget a b -> Widget a b
overlay f g constraintX constraintY = overlayDrawable (f constraintX constraintY) (g constraintX constraintY)

coloredBackgroud :: Color -> Widget MaxBounded MaxBounded -> Widget MaxBounded MaxBounded
coloredBackgroud = overlay . flexibleSquare

aspectRatio :: Double -> Widget MaxBounded MaxBounded -> Widget MaxBounded MaxBounded
aspectRatio goalRatio w xConstraint yConstraint
    | actualRatio > goalRatio = w (round $ goalRatio * fromIntegral yConstraint) yConstraint
    | otherwise = w xConstraint (round $ goalRatio * fromIntegral xConstraint)
  where
    actualRatio = fromIntegral xConstraint / fromIntegral yConstraint

marginForAxis :: (CInt -> CInt) -> Axis -> CInt -> Widget MaxBounded a -> Widget MaxBounded a
marginForAxis determineShift axis margin w maxX constraintY = shiftInAxis axis (determineShift margin) $
    w (subtractPixels maxX margin) constraintY

marginLeft :: CInt -> Widget MaxBounded a -> Widget MaxBounded a
marginLeft = marginForAxis id XAxis

marginTop :: CInt -> Widget a MaxBounded -> Widget a MaxBounded
marginTop = adjustForYAxis . marginForAxis id YAxis

marginRight :: CInt -> Widget MaxBounded a -> Widget MaxBounded a
marginRight = marginForAxis (const 0) XAxis

marginDown :: CInt -> Widget a MaxBounded -> Widget a MaxBounded
marginDown = adjustForYAxis . marginForAxis (const 0) YAxis

marginEach :: CInt -> CInt -> CInt -> CInt -> Widget MaxBounded MaxBounded -> Widget MaxBounded MaxBounded
marginEach top right down left = marginTop top . marginDown down . marginLeft left . marginRight right

marginAround :: CInt -> Widget MaxBounded MaxBounded -> Widget MaxBounded MaxBounded
marginAround x = marginEach x x x x

pairToV2 :: (a, a) -> V2 a
pairToV2 (x, y) = V2 x y

-- Getting the size of text does not perform actual side effects, so, unsafePerformIO should not cause trouble
-- Reworking the system to call this function in IO would be a significant pain
textSizeForFont :: Font -> Text -> V2 CInt
textSizeForFont font t = fmap fromIntegral $ pairToV2 $ System.IO.Unsafe.unsafePerformIO $ Font.size font t

text :: Font -> Text -> Widget ConstantSized ConstantSized
text font t () () = MkDrawable [DrawText font t (P $ V2 0 0)] (MkLayoutData $ textSizeForFont font t)

flexibleImage :: SDL.Texture -> Widget MaxBounded MaxBounded
flexibleImage texture xSize ySize =
    let size = V2 xSize ySize
    in MkDrawable [DrawTexture texture (SDL.Rectangle (P $ V2 0 0) size)] (MkLayoutData size)

image :: CInt -> CInt -> SDL.Texture -> Widget ConstantSized ConstantSized
image x y = limitSize x y . flexibleImage
