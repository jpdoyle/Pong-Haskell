module Game.Pong.Core where

import System.Random
import Control.Monad (when)

import Game.Kinematics(runKinematics)
import Game.Vector

bound :: (Num a,Ord a) => a -> a -> a -> a
bound low range = min (low+range) . max low

inward :: (Num a,Ord a) => a -> a -> a -> a -> a
inward low range def x
    | x < low       =  1
    | x > low+range = -1
    | otherwise     = def

data Direction = Up | Down | Stop deriving(Show,Read,Eq)
data Side = LeftSide | RightSide deriving(Show,Read,Eq)

dirToSignum :: Num a => Direction -> a
dirToSignum d = case d of
                    Up -> -1
                    Down -> 1
                    _ -> 0

data Paddle = Paddle { paddleLoc :: Vec2f } deriving(Show,Read)

boundPaddle :: Float -> Float -> Float -> Paddle -> Paddle
boundPaddle low height ph (Paddle (Vec2 x y)) = Paddle (Vec2 x y')
    where
        y' = bound (low+ph/2) (height-ph) y

tickPaddle :: Direction -> Float -> Float -> Paddle -> Paddle
tickPaddle d h dt (Paddle loc) = Paddle $ loc +. (dt *. Vec2 0 vy)
    where
        vy = 1.0*h*dirToSignum d

data Ball = Ball { ballLoc :: Vec2f, ballVel :: Vec2f }
    deriving(Show,Read)

boundBall :: Vec2f -> Vec2f -> Float -> Ball -> Ball
boundBall (Vec2 ox oy) (Vec2 w h) size (Ball (Vec2 lx ly)
                                             (Vec2 vx vy))
        = Ball newLoc newVel
    where
        newLoc = Vec2 (bound minx realw lx) (bound miny realh ly)
        newVel = Vec2 (xBounce*abs vx) (yBounce*abs vy)
        xBounce = inward minx realw (signum vx) lx
        yBounce = inward miny realh (signum vy) ly
        minx = ox+size/2
        miny = oy+size/2
        realw = w-size
        realh = h-size

tickBall :: Float -> Ball -> Ball
tickBall dt (Ball loc vel) = Ball newLoc newVel
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
            -- If both endpoints are within 90 degrees of the normal, they do
            -- not intersect.
            when ((relP1 `vdot` normal)*(relP2 `vdot` normal) > 0)
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

    pLeftDir,pRightDir :: Direction,
    pLeftScore,pRightScore :: Int,
    pHasFocus :: Bool,

    pRandom :: StdGen,
    pFactor :: Maybe Float,

    pRunning :: Bool,
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
        rp = Paddle $ Vec2 (v2x size - px) -- - (pw/2))
                              $ v2y rescale
                                *v2y (paddleLoc $ pRightPaddle p)
        b = Ball (rescale .*. ballLoc (pBall p))
                 (rescale .*. ballVel (pBall p))
        rescale = Vec2 (v2x size/v2x (pScreenSize p))
                       (v2y size/v2y (pScreenSize p))
        px = 0.1 * v2x size

randBallVel :: (Random a,Floating a) =>
               Vec2 a -> StdGen -> (Vec2 a,StdGen)
randBallVel v r = (v .*. Vec2 x y,r3)
    where
        x =  if flip then negate x' else x'
        (Vec2 x' y) = polarVec mag theta
        (flip,r3) = random r2
        (theta,r2) = randomR (-pi/4,pi/4) r1
        (mag,r1) = randomR (0.2,0.4) r -- (0.2,0.4) r

mkPong :: PongParams -> Pong
mkPong (PongParams size@(Vec2 w h) rand)
        = recalcSize size $ Pong (Vec2 1 1) vzero vzero lp rp b
                                 Stop Stop 0 0 True newRand Nothing
                                 False False
    where
        lp = Paddle (Vec2 0 (v2y mid))
        rp = Paddle (Vec2 0 (v2y mid))
        b = Ball mid startVel
        (startVel,newRand) = randBallVel (Vec2 1 1) rand
        mid = Vec2 0.5 0.5

isPongOver :: Pong -> Bool
isPongOver = pDone

pongTitle :: Pong -> String
pongTitle _ = "Pong!"

