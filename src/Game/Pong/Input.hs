module Game.Pong.Input where

import Control.Monad.SFML (SFML)
import qualified Control.Monad.SFML.Window as WM
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import SFML.Window.Event
import Game.Event
import Game.Vector

import Game.Pong.Core

data PongInput = PongInput {
    piEvents :: [Event],
    piLeft,piRight,piUp,piDown :: Bool,
    piA,piD,piW,piS :: Bool
}

grabPongInput :: G.RenderWindow -> SFML PongInput
grabPongInput w = do
    events <- allAvailableEvents w
    up <- WM.isKeyPressed W.KeyUp
    down <- WM.isKeyPressed W.KeyDown
    w <- WM.isKeyPressed W.KeyW
    s <- WM.isKeyPressed W.KeyS
    return $ PongInput events False False up down
                              False False w  s

processPongInput :: PongInput -> Pong -> Pong
processPongInput pi p = inputDir $ foldl processOne p $ piEvents pi
    where
        inputDir p = let focus = pHasFocus p in
                     p{pLeftDir  = if focus then ldir else Stop,
                       pRightDir = if focus then rdir else Stop}
        ldir = case (piW pi,piS pi) of
                (True,True) -> Stop
                (True,_)    -> Up
                (_,True)    -> Down
                _           -> Stop
        rdir = case (piUp pi,piDown pi) of
                (True,True) -> Stop
                (True,_)    -> Up
                (_,True)    -> Down
                _           -> Stop
        processOne p e = case e of
            SFEvent SFEvtClosed -> p{pDone = True}
            SFEvent (SFEvtResized w h) ->
                let fw = fromIntegral w
                    fh = fromIntegral h in
                recalcSize (Vec2 fw fh) p
            SFEvent SFEvtLostFocus -> p{pRunning = False}
            -- SFEvent SFEvtGainedFocus -> p{pHasFocus = True}
            SFEvent (SFEvtKeyPressed{code=key}) -> case key of
                W.KeySpace -> p{pRunning=not $ pRunning p}
                _ -> p
            _ -> p

