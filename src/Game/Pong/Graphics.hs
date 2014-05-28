module Game.Pong.Graphics where

import Data.Functor

import Game.Pong.Core
import Game.Util (sfVec2f,forceCleanup)
import Game.Vector

import Control.Monad.SFML (SFML)
import qualified SFML.Graphics as G
import qualified SFML.Window as W
import qualified Control.Monad.SFML.Window as WM
import qualified Control.Monad.SFML.Graphics as GM
import SFML.Graphics.Color (black,white)

data PongContext = PongContext {
    pcWin :: G.RenderWindow,
    pcFont :: G.Font
}

dashedLine :: (Vec2f,Vec2f) -> Float -> [W.Vec2f]
dashedLine (p1,p2) dashLen = verts
    where
        verts = take (nSegs*2) $ sfVec2f
                               <$> (makeDash
                                    =<< iterate (+. step) init)
        makeDash v = [v,v+.dash]
        step = stepLen *. vnorm diff
        dash = dashLen *. vnorm diff
        init = p1 +. ((1/6+initFact/2)*stepLen) *. vnorm diff
        (nSegs,initFact) = properFraction $ vmag diff / stepLen
        stepLen = dashLen*1.5
        diff = p2 -. p1

setVAContents :: G.VertexArray -> [G.Vertex] -> SFML ()
setVAContents va vs = do
    GM.clearVA va
    sequence_ $ GM.appendVA va <$> vs

drawScores :: (Int,Int) -> PongContext -> SFML ()
drawScores (l,r) (PongContext wnd fnt) = do
    (W.Vec2f w h) <- GM.getViewSize =<< GM.getView wnd
    let mid = w/2
    let (lx,rx) = (0.8*mid,1.2*mid)
    let y = negate $ 0.01*h -- .001*h
    txt <- GM.createText
    GM.setTextFont txt fnt
    GM.setTextCharacterSize txt $ fromIntegral $ floor (0.1*h)
    let (lText,rText) = (show l,show r)

    GM.setTextString txt lText
    (G.FloatRect _ _ tw th) <- GM.getTextLocalBounds txt
    GM.setOrigin txt (W.Vec2f tw 0)
    GM.setPosition txt $ W.Vec2f lx y
    GM.drawText wnd txt Nothing

    GM.setTextString txt rText
    (G.FloatRect _ _ tw th) <- GM.getTextLocalBounds txt
    GM.setOrigin txt (W.Vec2f tw 0)
    GM.setPosition txt $ W.Vec2f rx y
    GM.drawText wnd txt Nothing

drawMidLine :: G.RenderWindow -> SFML ()
drawMidLine wnd = do
    (W.Vec2f w h) <- GM.getViewSize =<< GM.getView wnd
    let mid = w/2
    va <- GM.createVA
    GM.setPrimitiveType va G.Lines
    setVAContents va
                     $ (\p -> G.Vertex p white (W.Vec2f 0 0))
                     <$> dashedLine (Vec2 mid 0,Vec2 mid h) 80
    GM.drawVertexArray wnd va Nothing

drawPong :: Pong -> PongContext -> SFML ()
drawPong p ctx@(PongContext wnd fnt) = forceCleanup $ do
    let (Vec2 w h) = pScreenSize p
    view <- GM.viewFromRect (G.FloatRect 0 0 w h)
    GM.setView wnd view
    GM.clearRenderWindow wnd black

    drawScores (pLeftScore p,pRightScore p) ctx

    drawMidLine wnd

    rectShape <- GM.createRectangleShape
    GM.setSize rectShape (sfVec2f $ pPaddleSize p)

    let halfsize = 0.5 *. pPaddleSize p
    GM.setPosition rectShape $ sfVec2f
                    $ paddleLoc (pLeftPaddle p) -. halfsize
    GM.drawRectangle wnd rectShape Nothing

    GM.setPosition rectShape $ sfVec2f
                    $ paddleLoc (pRightPaddle p) -. halfsize
    GM.drawRectangle wnd rectShape Nothing

    GM.setSize rectShape (sfVec2f $ pBallSize p)
    let halfsize = 0.5 *. pBallSize p
    GM.setPosition rectShape $ sfVec2f
                    $ ballLoc (pBall p) -. halfsize
    GM.drawRectangle wnd rectShape Nothing

