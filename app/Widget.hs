{-# LANGUAGE DataKinds, TypeFamilyDependencies, NoStarIsType, TupleSections #-}

module Widget
    ( WidgetSizeDependency(..)
    , Widget
    , row
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
    , nextTo
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
    , Color
    , shiftPrimitives
    , combineRect
    , nextToHorizontally
    , overlayDrawable
    , emptyDrawable
    , shiftX
    , shiftY
    , drawableWidth
    , drawableHeight
    , xCoord
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

expandSideways :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> SDL.Rectangle CInt
expandSideways = combineRect min min (+) max

-- Todo: Rewrite this using the new functions from Drawable
besides :: Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
besides f g _ yConstraint = MkDrawable (fPrimitives ++ shiftPrimitives (V2 fSize 0) gPrimitives) $
    MkLayoutData $ expandSideways fBoundingBox gBoundingBox
  where
    rectWidth (SDL.Rectangle _ size) = xCoord size

    (MkDrawable fPrimitives (MkLayoutData fBoundingBox)) = f () yConstraint
    (MkDrawable gPrimitives (MkLayoutData gBoundingBox)) = g () yConstraint
    fSize = rectWidth fBoundingBox

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- Subtracts two `CInt`s, but ensures the result is not negative
subtractPixels :: CInt -> CInt -> CInt
subtractPixels = max 0 .: (-)

nextTo :: Widget ConstantSized a -> Widget MaxBounded a -> Widget MaxBounded a
nextTo f g maxBound yConstraint = nextToHorizontally fDrawable $ shiftX fWidth $ g (subtractPixels maxBound fWidth) yConstraint
  where
    fDrawable :: Drawable
    fDrawable = f () yConstraint

    fWidth = drawableWidth fDrawable

-- TODO: below, column, margin

empty :: Widget a b
empty _ _ = emptyDrawable

row :: [Widget ConstantSized a] -> Widget ConstantSized a
row = foldr besides empty

flexibleSquare :: Color -> Widget MaxBounded MaxBounded
flexibleSquare color width height =
    let rect = SDL.Rectangle (P $ V2 0 0) (V2 width height)
    in MkDrawable [Square color rect] $ MkLayoutData rect

limitSizeX :: CInt -> Widget MaxBounded a -> Widget ConstantSized a
limitSizeX size f _ = f size

limitSizeY :: CInt -> Widget a MaxBounded -> Widget a ConstantSized
limitSizeY size f = flip $ const $ flip f size

limitSize :: CInt -> CInt -> Widget MaxBounded MaxBounded -> Widget ConstantSized ConstantSized
limitSize limitX limitY = limitSizeY limitY . limitSizeX limitX

-- It would be nice to reduce this code duplication between axis
-- (but refactoring might require more type families and possibily a singleton)
alignRatioX :: Double -> Widget ConstantSized a -> Widget MaxBounded a
alignRatioX ratio w availableWidth yConstraint = shiftX (round $ ratio * fromIntegral (availableWidth - drawableWidth drawable)) drawable
  where
    drawable = w () yConstraint

alignRatioY :: Double -> Widget a ConstantSized -> Widget a MaxBounded
alignRatioY ratio w xConstraint availableHeight = shiftY (round $ ratio * fromIntegral (availableHeight - drawableHeight drawable)) drawable
  where
    drawable = w xConstraint ()

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
