{-# LANGUAGE DataKinds, TypeFamilies, TypeFamilyDependencies, NoStarIsType #-}

module Widget where

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
expandSideways (SDL.Rectangle (P (V2 x y)) (V2 w h)) (SDL.Rectangle (P (V2 x' y')) (V2 w' h')) = 
    SDL.Rectangle (P $ V2 (min x x') (min y y')) (V2 (w + w') (max h h'))

besides :: Widget ConstantSized a -> Widget ConstantSized a -> Widget ConstantSized a
besides f g _ yConstraint = MkDrawable (fPrimitives ++ shiftPrimitives (V2 fSize 0) gPrimitives) $
    MkLayoutData $ expandSideways fBoundingBox gBoundingBox
  where
    width (SDL.Rectangle _ size) = xCoord size

    (MkDrawable fPrimitives (MkLayoutData fBoundingBox)) = f () yConstraint
    (MkDrawable gPrimitives (MkLayoutData gBoundingBox)) = g () yConstraint
    fSize = width fBoundingBox

empty :: Widget ConstantSized b
empty _ _ = MkDrawable [] $ MkLayoutData $ SDL.Rectangle (P $ V2 0 0) (V2 0 0)

row :: [Widget ConstantSized a] -> Widget ConstantSized a
row = foldr besides empty

flexibleSquare :: Color -> Widget MaxBounded MaxBounded
flexibleSquare color width height =
    let rect = SDL.Rectangle (P $ V2 0 0) (V2 width height)
    in MkDrawable [Square color rect] $ MkLayoutData rect

fixedSize :: CInt -> Widget MaxBounded a -> Widget ConstantSized a
fixedSize size f _ = f size

fixedSizeY :: CInt -> Widget a MaxBounded -> Widget a ConstantSized
fixedSizeY size f = flip $ const $ flip f size

limit :: CInt -> CInt -> Widget MaxBounded MaxBounded -> Widget ConstantSized ConstantSized
limit limitX limitY = fixedSizeY limitY . fixedSize limitX

alignLeft :: Widget ConstantSized a -> Widget MaxBounded a
alignLeft w = const $ w ()

alignTop :: Widget a ConstantSized -> Widget a MaxBounded
alignTop w = flip $ const $ flip w $ ()

-- TODO: alignRatio, alignRight, alignLeft
