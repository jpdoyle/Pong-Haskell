module Game.Util where

import Control.Monad.SFML
import qualified Control.Monad.SFML.System as S

while :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
while p start step = go start
    where
        go x = if p x then
                   step x >>= go
               else
                   return x

while_ :: Monad m => (a -> Bool) -> m a -> m a
while_ p step = go
    where
        go = do
                val <- step
                if p val then
                    go
                else
                    return val

fpsCap :: Int -> a -> (a -> SFML a) -> SFML (Int,Int,a)
fpsCap fps start step = let msPerFrame = 1000 `div` fps
                            iConvert :: (Integral a,Num c) => a -> c
                            iConvert = fromInteger . toInteger in
    do
        clk <- S.createClock
        startTime <- S.getElapsedTime clk
        next <- step start
        endTime <- S.getElapsedTime clk
        let desiredTime = iConvert msPerFrame
        let actualTime = endTime - startTime
        let timeLeft = max 0 $ desiredTime - actualTime
        S.sfSleep timeLeft
        lastTime <- S.getElapsedTime clk
        return (iConvert actualTime,iConvert (lastTime-startTime),next)

fpsCap_ :: Int -> SFML a -> SFML (Int,Int,a)
fpsCap_ fps step = fpsCap fps undefined (const step)

