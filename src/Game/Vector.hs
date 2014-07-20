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

class CartesianVec v where
    (.*.) :: v -> v -> v

type Vec2 a = (a,a);

instance Num a => CartesianVec (Vec2 a) where
    (ux,uy) .*. (vx,vy) = (ux*vx,uy*vy)

type Vec2i = Vec2 Int
type Vec2f = Vec2 Float
type Vec2d = Vec2 Double

polarVec :: Floating a => a -> a -> Vec2 a
polarVec r theta = (r*cos theta,r*sin theta)

instance Num a => Vector a (Vec2 a) where
    vplus (a,b) (c,d) = (a+c,b+d)
    vnegate (x,y) = (negate x,negate y)
    vzero = (0,0)
    vscale s (x,y) = (s*x,s*y)
    vdot (a,b) (c,d) = (a*c) + (b*d)

instance Eq a => Eq (Vec2 a) where
    (x1,y1) == (x2,y2) = (x1 == y1) && (x2 == y2)

