module Game.Util where

import Control.Monad.SFML
import qualified SFML.System as S
import qualified Control.Monad.SFML.System as SM
import Game.Vector

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


milliseconds :: Integral a => a -> a
milliseconds = (*1000) . abs

seconds :: (RealFrac f,Integral i) => f -> i
seconds = floor . (*1e6)

fpsCap :: Int -> a -> (a -> SFML a) -> SFML (Int,Int,a)
fpsCap fps start step = let usPerFrame = 1000000 `div` fps
                            iConvert :: (Integral a,Num c) => a -> c
                            iConvert = fromInteger . toInteger in
    do
        clk <- SM.createClock
        startTime <- SM.getElapsedTime clk
        next <- step start
        endTime <- SM.getElapsedTime clk
        let desiredTime = iConvert usPerFrame
        let actualTime = endTime - startTime
        let timeLeft = max 0 $ desiredTime - actualTime
        SM.sfSleep timeLeft
        lastTime <- SM.getElapsedTime clk
        return (iConvert actualTime `div` 1000,iConvert (lastTime-startTime) `div` 1000,next)

fpsCap_ :: Int -> SFML a -> SFML (Int,Int,a)
fpsCap_ fps step = fpsCap fps undefined (const step)

sfVec2f :: Vec2f -> S.Vec2f
sfVec2f (Vec2 x y) = S.Vec2f x y

forceCleanup :: SFML () -> SFML ()
forceCleanup = liftIO . runSFML

