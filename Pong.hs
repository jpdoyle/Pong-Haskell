-- module Game.Pong where
module Main where

import Control.Monad
import Game.Util
import Game.Event
import Game.Pong
import qualified Graphics.UI.SDL as S

pongOnce :: Double -> Pong -> IO Pong
pongOnce dt p = do
    events <- allAvailableEvents
    let newP = (tickPong dt . processPongEvents events) p
    drawPong newP
    return newP

runPong :: Int -> IO ()
runPong fps = void $ while (not . isPongOver) loadPong $
    \p -> do
            let dt = fromRational $ recip $ toRational fps :: Double
            (timeTaken,totalTime,newP) <- fpsCap_ fps $ pongOnce dt p
            let title = pongTitle newP
            setTitle $ title ++ ": " ++ show timeTaken ++ "/" ++ show totalTime
            return newP

main :: IO ()
main = S.withInit [S.InitVideo,S.InitTimer] $ do
    S.setVideoMode 300 300 32 [S.SWSurface,S.Resizable]
    runPong 40
    -- void $ while_ (not . isPongOver) $ do
    --     (timeTaken,totalTime,continue) <- fpsCap_ 60 checkEvents
    --     putStrLn $ show timeTaken ++ "/" ++ show totalTime
    --     return continue

