{-# LANGUAGE DataKinds, TypeFamilyDependencies, NoStarIsType, TupleSections #-}

module Widget
    ( WidgetSizeDependency(..)
    , Widget
    , beside
    , row
    , below
    , column
    , toTheLeftOf
    , above
    , flexibleSquare
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
    ) where

import Data.Kind (Type)

import Foreign.C.Types (CInt)

import SDL (V2(..), Point(..))
import qualified SDL (Rectangle(..))

import Drawable
    ( Drawable(..)
    , LayoutData(..)
    , DrawingPrimitive(..)
    , Axis(..)
    , Color
    , nextToForAxis
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

besideForAxis :: Axis -> Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
besideForAxis axis f g _ yConstraint  = nextToForAxis axis fDrawable $ shiftInAxis axis (drawableSizeForAxis axis fDrawable) gDrawable
  where
    fDrawable, gDrawable :: Drawable
    fDrawable = f () yConstraint
    gDrawable = g () yConstraint

beside :: Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
beside = besideForAxis XAxis

adjustForYAxis
    :: ((b' -> a' -> c') -> (b'' -> a'' -> c'') -> b -> a -> c)
    ->  (a' -> b' -> c') -> (a'' -> b'' -> c'') -> a -> b -> c
adjustForYAxis f a b = flip $ f (flip a) (flip b)

below :: Widget a ConstantSized -> Widget a ConstantSized -> Widget a ConstantSized
below = adjustForYAxis $ besideForAxis YAxis

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

above :: Widget a ConstantSized -> Widget a MaxBounded -> Widget a MaxBounded
above = adjustForYAxis $ adjacentForAxis YAxis

-- TODO: margin

alignRatioForAxis
    :: Axis
    -> (WidgetDependency a -> Drawable)
    -> Double
    -> Widget MaxBounded a
alignRatioForAxis axis getDrawable ratio availableSize otherConstraint =
    let drawable = getDrawable otherConstraint
    in shiftInAxis axis (round $ ratio * fromIntegral (availableSize - drawableSizeForAxis axis drawable)) drawable

alignRatioX :: Double -> Widget ConstantSized a -> Widget MaxBounded a
alignRatioX r w = alignRatioForAxis XAxis (w ()) r

alignRatioY :: Double -> Widget a ConstantSized -> Widget a MaxBounded
alignRatioY r w = flip $ alignRatioForAxis YAxis (flip w ()) r

empty :: Widget a b
empty _ _ = emptyDrawable

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
