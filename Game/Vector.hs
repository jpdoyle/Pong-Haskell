module Game.Vector where

-- A Euclidian vector.
-- Minimal complete definition requires 'vzero', 'vscale','vdot', and
-- any two of 'vplus', 'vminus', and 'vnegate'
class Functor v => Vector v where
    vplus   :: Num a => v a -> v a -> v a
    vminus  :: Num a => v a -> v a -> v a
    vnegate :: Num a => v a -> v a
    vzero   :: Num a => v a
    vscale  :: Num a => a -> v a -> v a
    vdot    :: Num a => v a -> v a -> a

    vplus u v  = u `vminus` vnegate v
    vminus u v = u `vplus` vnegate v
    vnegate v  = vzero `vminus` v

(*.) :: (Vector v,Num a) => a -> v a -> v a
(*.) = vscale
(+.) :: (Vector v,Num a) => v a -> v a -> v a
(+.) = vplus
(-.) :: (Vector v,Num a) => v a -> v a -> v a
(-.) = vminus
infixl 7 *.
infixl 6 +.,-.

vmagSquared :: (Vector v,Num a) => v a -> a
vmagSquared v = vdot v v

vmag :: (Vector v,Floating a) => v a -> a
vmag = sqrt . vmagSquared

vnorm :: (Vector v,Floating a) => v a -> v a
vnorm v = recip (vmag v) *. v

vproject :: (Vector v,Fractional a) => v a -> v a -> v a
vproject u v  = ((u `vdot` v)/(v `vdot` v)) *. v

data Vec2 a = Vec2 { v2x :: a,v2y :: a } deriving(Show)

type Vec2i = Vec2 Int
type Vec2f = Vec2 Float
type Vec2d = Vec2 Double

polarVec :: Floating a => a -> a -> Vec2 a
polarVec r theta = Vec2 (r*cos theta) (r*sin theta)

instance Functor Vec2 where
    fmap f (Vec2 x y) = Vec2 (f x) (f y)

instance Vector Vec2 where
    
    vplus (Vec2 a b) (Vec2 c d) = Vec2 (a+c) (b+d)
    vnegate (Vec2 x y) = Vec2 (negate x) (negate y)
    vzero = Vec2 0 0
    vscale s (Vec2 x y) = Vec2 (s*x) (s*y)
    vdot (Vec2 a b) (Vec2 c d) = (a*c) + (b*d)

