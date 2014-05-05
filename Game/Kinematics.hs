module Game.Kinematics where

import Game.Vector

runKinematics :: (Vector v,Fractional a) => v a -> v a -> v a -> a -> (v a,v a)
runKinematics acc vel loc dt = (newVel,newLoc)
    where
        newVel = vel +. dt *. acc
        newLoc = loc +. dt *. avgVel
        avgVel = (1/2) *. (newVel +. vel)

