module Game.Util where

import qualified Graphics.UI.SDL as S

while :: Monad m => (a -> Bool) -> m a -> (a -> m a) -> m a
while p start step = go start
    where
        go act = do
                    x <- act
                    if p x then
                        go $ step x
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

fpsCap :: Int -> IO a -> (a -> IO a) -> IO (Int,Int,a)
fpsCap fps start step = let msPerFrame = 1000 `div` fps
                            iConvert :: (Integral a,Num c) => a -> c
                            iConvert = fromInteger . toInteger in
    do
        startTime <- S.getTicks
        startVal <- start
        next <- step startVal
        endTime <- S.getTicks
        let desiredTime = iConvert msPerFrame
        let actualTime = endTime - startTime
        let timeLeft = max 0 $ desiredTime - actualTime
        S.delay timeLeft
        lastTime <- S.getTicks
        return (iConvert actualTime,iConvert (lastTime-startTime),next)

fpsCap_ :: Int -> IO a -> IO (Int,Int,a)
fpsCap_ fps step = fpsCap fps (return undefined) (const step)

setTitle :: String -> IO ()
setTitle s = S.rawSetCaption (Just s) Nothing

