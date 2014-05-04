-- module Game.Pong where
module Main where

import Control.Monad
import qualified Graphics.UI.SDL as S

while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
while p step start = go start
    where
        go x = if p x then
                step start >>= go
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

fpsCap :: Int -> (a -> IO a) -> a -> IO (Int,Int,a)
fpsCap fps step start = let msPerFrame = 1000 `div` fps
                            iConvert :: (Integral a,Num c) => a -> c
                            iConvert = fromInteger . toInteger in
    do
        startTime <- S.getTicks
        next <- step start
        endTime <- S.getTicks
        let desiredTime = iConvert msPerFrame
        let actualTime = endTime - startTime
        let timeLeft = max 0 $ desiredTime - actualTime
        S.delay timeLeft
        lastTime <- S.getTicks
        return (iConvert actualTime,iConvert (lastTime-startTime),next)

fpsCap_ :: Int -> IO a -> IO (Int,Int,a)
fpsCap_ fps step = fpsCap fps (const step) undefined

checkEvents :: IO Bool
checkEvents = do
                e <- lastEvent
                case e of
                    S.Quit -> return False
                    _      -> return True
    where
        lastEvent = while_ shouldEnd S.pollEvent
        shouldEnd e = case e of
                        S.Quit    -> False
                        S.NoEvent -> False
                        _         -> True

main :: IO ()
main = S.withInit [S.InitVideo,S.InitTimer] $ do
    S.setVideoMode 300 300 32 [S.SWSurface,S.Resizable]
    void $ while_ id $ do
        (timeTaken,totalTime,continue) <- fpsCap_ 60 checkEvents
        putStrLn $ show timeTaken ++ "/" ++ show totalTime
        return continue

