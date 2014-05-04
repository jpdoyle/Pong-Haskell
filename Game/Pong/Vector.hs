module Game.Pong.Vector where

-- A Euclidian vector.
--
-- Minimal complete definition requires 'vzero', 'vscale','vdot', and
-- any two of 'vadd', 'msub', and 'vnegate'
class Vector v where
    vadd,vsub   :: v a -> v a -> v a
    vnegate     :: v a -> v a
    vzero       :: v a
    vmag        :: v a -> a
    vmagSquared :: v a -> a
    vscale      :: a -> v a -> v a
    vdot        :: v a -> v a -> a
    vnorm       :: v a -> v a
    vproject    :: v a -> v a -> v a

    vadd u v      = vsub u $ vnegate v
    vsub u v      = vadd u $ vnegate v
    vnegate v     = vzero `vsub` v
    vmagSquared v = vdot v v
    vmag          = sqrt . vmagSquared
    vnorm v       = (1.0/vmag v) `vscale` v
    vproject u v  = ((u `vdot` v)/(v `vdot` v)) `vscale` v

data Vec2 a = Vec2 a a

polarVec :: Floating a => a -> a -> Vec2 a
polarVec r theta = Vec2 (r*cos theta) (r*sin theta)

instance Num a => Vector Vec2 where
    vadd (Vec2 a b) (Vec2 c d) = Vec2 (a+c) (b+d)
    vnegate (Vec2 x y) = Vec2 (negate x) (negate y)
    vzero = Vec2 0 0
    vscale s (Vec2 x y) = Vec2 (s*x) (s*y)
    vdot (Vec2 a b) (Vec2 c d) = (a*c) + (b*d)

