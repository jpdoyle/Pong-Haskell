module Game.Kinematics where

import Game.Vector

runKinematics :: (Vector a v,Fractional a) => v -> v -> v -> a -> (v,v)
runKinematics acc vel loc dt = (newVel,newLoc)
    where
        newVel = vel +. dt *. acc
        newLoc = loc +. dt *. avgVel
        avgVel = (1/2) *. (newVel +. vel)

