-- module Game.Pong where
module Main where

import Control.Monad
import System.Random (newStdGen)
import Game.Vector
import Game.Util
import Game.Event
import Game.Pong
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified SFML.Window as W
import qualified Control.Monad.SFML.Graphics as GM
import qualified Control.Monad.SFML.Window as WM

pongOnce :: Double -> G.RenderWindow -> Pong -> SFML Pong
pongOnce dt w p = do
    events <- liftIO $ allAvailableEvents w
    let newP = (tickPong dt . processPongEvents events) p
    drawPong newP w
    return newP

runPong :: PongParams -> Int -> G.RenderWindow -> SFML ()
runPong params fps wnd = void $ game (mkPong params)
    where
        game init = while (not . isPongOver) init iterOnce
        iterOnce p = do
            let dt = fromRational $ recip $ toRational fps :: Double
            (timeTaken,totalTime,newP) <- fpsCap fps p
                                          $ pongOnce dt wnd
            let title = pongTitle newP
            liftIO $ W.setWindowTitle wnd
                   $ title ++ ": " ++ show timeTaken
                           ++ "/"  ++ show totalTime
            return newP

main :: IO ()
main = runSFML $ do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- GM.createRenderWindow (W.VideoMode 640 480 32)
           "Pong" [W.SFDefaultStyle] ctxSettings
    rnd <- liftIO newStdGen
    runPong (PongParams (Vec2 640 480) rnd) 40 wnd
    -- void $ while_ (not . isPongOver) $ do
    --     (timeTaken,totalTime,continue) <- fpsCap_ 60 checkEvents
    --     putStrLn $ show timeTaken ++ "/" ++ show totalTime
    --     return continue

