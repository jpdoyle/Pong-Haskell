{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.Vector where

-- A Euclidian vector.
-- Minimal complete definition requires 'vzero', 'vscale','vdot', and
-- any two of 'vplus', 'vminus', and 'vnegate'
class Vector a v | v -> a where
    vplus   :: v -> v -> v
    vminus  :: v -> v -> v
    vnegate :: v -> v
    vzero   :: v
    vscale  :: a -> v -> v
    vdot    :: v -> v -> a

    vplus u v  = u `vminus` vnegate v
    vminus u v = u `vplus` vnegate v
    vnegate v  = vzero `vminus` v

(*.) :: Vector a v => a -> v -> v
(*.) = vscale
(+.) :: Vector a v => v -> v -> v
(+.) = vplus
(-.) :: Vector a v => v -> v -> v
(-.) = vminus
infixl 7 *.
infixl 6 +.,-.

vmagSquared :: Vector a v => v -> a
vmagSquared v = vdot v v

vmag :: (Vector a v,Floating a) => v -> a
vmag = sqrt . vmagSquared

vnorm :: (Vector a v,Floating a) => v -> v
vnorm v = recip (vmag v) *. v

vproject :: (Vector a v,Fractional a) => v -> v -> v
vproject u v  = ((u `vdot` v)/(v `vdot` v)) *. v

data Vec2 a = Vec2 { v2x :: a,v2y :: a } deriving(Show)

type Vec2i = Vec2 Int
type Vec2f = Vec2 Float
type Vec2d = Vec2 Double

polarVec :: Floating a => a -> a -> Vec2 a
polarVec r theta = Vec2 (r*cos theta) (r*sin theta)

instance Num a => Vector a (Vec2 a) where
    vplus (Vec2 a b) (Vec2 c d) = Vec2 (a+c) (b+d)
    vnegate (Vec2 x y) = Vec2 (negate x) (negate y)
    vzero = Vec2 0 0
    vscale s (Vec2 x y) = Vec2 (s*x) (s*y)
    vdot (Vec2 a b) (Vec2 c d) = (a*c) + (b*d)

