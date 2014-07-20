module Game.Pong.Run where

import System.Random (newStdGen)
import Control.Monad

import Control.Monad.SFML
import qualified SFML.Window as W
import qualified Control.Monad.SFML.Graphics as GM

import Game.Vector
import Game.Util
import Game.Event
import Game.Pong.Core
import Game.Pong.Input
import Game.Pong.Graphics
import Game.Pong.Logic
import Paths_Pong (getDataFileName)

pongOnce :: Float -> PongContext -> Pong -> SFML Pong
pongOnce dt ctx@(PongContext w _) p = do
    input <- grabPongInput w
    let newP = (tickPong dt . processPongInput input) p
    drawPong newP ctx
    GM.display w
    return newP

pongLoop :: PongParams -> Int -> PongContext -> SFML ()
pongLoop params fps ctx@(PongContext wnd fnt) = void
                                                $ game (mkPong params)
    where
        game init = while (not . isPongOver) init iterOnce
        iterOnce p = do
            let dt = fromRational $ recip $ toRational fps :: Float
            (timeTaken,totalTime,newP) <- fpsCap fps p
                                          $ pongOnce dt ctx
            let title = pongTitle newP
            GM.setWindowTitle wnd
                   $ title ++ ": " ++ show timeTaken
                           ++ "ms/"  ++ show totalTime
                           ++ "ms"
            return newP

runPong :: SFML ()
runPong =  do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- GM.createRenderWindow (W.VideoMode 640 480 32)
           "Pong" [W.SFDefaultStyle] ctxSettings -- [W.SFTitlebar,W.SFClose]
    fntPath <- liftIO $ getDataFileName "MAG.ttf"
    fnt <- GM.fontFromFile fntPath
    rnd <- liftIO newStdGen
    pongLoop (PongParams (640,480) rnd) 60 (PongContext wnd fnt)

