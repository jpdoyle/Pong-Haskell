module Game.Pong where

import System.Random

import Game.Event (Event(..))
import Game.Vector
import Game.Kinematics (runKinematics)
import Control.Monad.SFML (SFML)
import qualified SFML.Graphics as G
import SFML.Window (SFEvent(SFEvtClosed))
import qualified Control.Monad.SFML.Graphics as GM

data Paddle = Paddle { paddleLoc :: Vec2f } deriving(Show)

data Ball = Ball { ballLoc :: Vec2f, ballVel :: Vec2f } deriving(Show)

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
} deriving(Show)

pongRand :: (StdGen -> (a,StdGen)) -> Pong -> (a,Pong)
pongRand f p = (r,p{pRandom=newRand})
    where
        (r,newRand) = f $ pRandom p

data PongParams = PongParams {
    ppScreenSize :: Vec2f,
    ppRandom :: StdGen
}

mkPong :: PongParams -> Pong
mkPong (PongParams size@(Vec2 w h) rand)
        = Pong size psize bsize lp rp b newRand False
    where
        psize = Vec2 (0.1*v2x size) (0.2*v2y size)
        bsize = Vec2 bs bs
        bs = 0.2 * v2y psize
        lp = Paddle (Vec2 px (v2y mid))
        rp = Paddle (Vec2 (v2x size - px) (v2y mid))
        b = Ball mid startVel
        (startVel,newRand) = let (theta,r1) = randomR (0,2*pi) rand
                                 (mag,r2)   = randomR (10,30)  r1 in
                             (polarVec mag theta,r2)
        px = 0.1 * v2x size
        mid = 0.5 *. size

isPongOver :: Pong -> Bool
isPongOver = pDone

drawPong :: Pong -> G.RenderWindow -> SFML ()
drawPong _ _ = return ()

processPongEvents :: [Event] -> Pong -> Pong
processPongEvents es p = foldl processOne p es
    where
        processOne p e = case e of
            SFEvent SFEvtClosed -> p{pDone = True}
            _ -> p

tickPong :: Double -> Pong -> Pong
tickPong _ = id

pongTitle :: Pong -> String
pongTitle _ = "Pong!"

