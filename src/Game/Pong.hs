module Game.Pong where

import System.Random

import Game.Event (Event(..))
import Game.Util (sfVec2f)
import Game.Vector
import Game.Kinematics (runKinematics)
import Control.Monad.SFML (SFML)
import qualified SFML.Graphics as G
import SFML.Window.Event
import qualified Control.Monad.SFML.Graphics as GM
import SFML.Graphics.Color (black,white)

data Paddle = Paddle { paddleLoc :: Vec2f } deriving(Show,Read)

data Ball = Ball { ballLoc :: Vec2f, ballVel :: Vec2f }
    deriving(Show,Read)

tickBall :: Ball -> Float -> Ball
tickBall (Ball loc vel) dt = Ball newLoc newVel
    where
        (newVel,newLoc) = runKinematics vzero vel loc dt

data Line v = Line { lPoint :: v,lNormal :: v }

-- Returns t in [0,1] such that the point (t*(p2-p1) + p1) is on line
-- l.
segLineIntersect :: Vec2f -> Vec2f -> Line Vec2f -> Maybe Float
segLineIntersect p1 p2 (Line point normal) = intersect
    where
        intersect = do
            let relP1 = p1 -. point
                relP2 = p2 -. point
            -- If one endpoint is within 90 degrees of the normal and
            -- the other is not, they intersect
            if (relP1 `vdot` normal)*(relP2 `vdot` normal) <= 0 then
                Just ()
            else
                Nothing
            -- Define the segment as p = p1 + (p2-p1)*t, 0 <= t <= 1
            let diff = p2 -. p1
            -- (p - point) `dot` normal = 0 at intersection
            -- (p1 + diff*t - point) `dot` normal = 0
            -- (p1 - point) `dot` normal + t*(diff `dot` normal) = 0
            -- t = -(relP1 `dot` normal)/(diff `dot` normal)
            return $ negate $ (relP1 `vdot` normal)
                              /(diff `vdot` normal)

data Pong = Pong {
    pScreenSize :: Vec2f,
    pPaddleSize :: Vec2f,
    pBallSize   :: Vec2f,

    pLeftPaddle  :: Paddle,
    pRightPaddle :: Paddle,
    pBall        :: Ball,

    pRandom :: StdGen,

    pDone :: Bool
} deriving(Show,Read)

pongRand :: (StdGen -> (a,StdGen)) -> Pong -> (a,Pong)
pongRand f p = (r,p{pRandom=newRand})
    where
        (r,newRand) = f $ pRandom p

data PongParams = PongParams {
    ppScreenSize :: Vec2f,
    ppRandom :: StdGen
}

recalcSize :: Vec2f -> Pong -> Pong
recalcSize size p = p{pScreenSize  = size,
                      pPaddleSize  = psize,
                      pBallSize    = bsize,
                      pLeftPaddle  = lp,
                      pRightPaddle = rp,
                      pBall        = b}
    where
        psize = Vec2 pw ph
        pw = ph/3
        ph = 0.1 * v2y size
        bsize = Vec2 bs bs
        bs = 0.2 * v2y psize
        lp = Paddle $ Vec2 px $ v2y rescale
                                *v2y (paddleLoc $ pLeftPaddle p)
        rp = Paddle $ Vec2 (v2x size - px - (pw/2))
                              $ v2y rescale
                                *v2y (paddleLoc $ pRightPaddle p)
        b = Ball (rescale .*. ballLoc (pBall p))
                 (rescale .*. ballVel (pBall p))
        (Vec2 a b) .*. (Vec2 c d) = Vec2 (a*c) (b*d)
        rescale = Vec2 (v2x size/v2x (pScreenSize p))
                       (v2y size/v2y (pScreenSize p))
        px = 0.1 * v2x size

mkPong :: PongParams -> Pong
mkPong (PongParams size@(Vec2 w h) rand)
        = recalcSize size $ Pong (Vec2 1 1) vzero vzero lp rp b newRand False
    where
        lp = Paddle (Vec2 0 (v2y mid))
        rp = Paddle (Vec2 0 (v2y mid))
        b = Ball mid startVel
        (startVel,newRand) = let (theta,r1) = randomR (0,2*pi) rand
                                 (mag,r2)   = randomR (0.1,0.4)  r1 in
                             (polarVec mag theta,r2)
        mid = Vec2 0.5 0.5

isPongOver :: Pong -> Bool
isPongOver = pDone

drawPong :: Pong -> G.RenderWindow -> SFML ()
drawPong p wnd = do
    let (Vec2 w h) = pScreenSize p
    view <- GM.viewFromRect (G.FloatRect 0 0 w h)
    GM.setView wnd view
    GM.clearRenderWindow wnd black

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

processPongEvents :: [Event] -> Pong -> Pong
processPongEvents es p = foldl processOne p es
    where
        processOne p e = case e of
            SFEvent SFEvtClosed -> p{pDone = True}
            SFEvent (SFEvtResized w h) ->
                let fw = fromIntegral w
                    fh = fromIntegral h in
                recalcSize (Vec2 fw fh) p
            _ -> p

tickPong :: Double -> Pong -> Pong
tickPong _ = id

pongTitle :: Pong -> String
pongTitle _ = "Pong!"

