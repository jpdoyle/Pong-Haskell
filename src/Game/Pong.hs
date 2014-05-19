module Game.Pong where

import Game.Event (Event)
import Game.Vector
import Game.Kinematics (runKinematics)

data Paddle = Paddle { paddleLoc :: Vec2f } deriving(Show)

data Ball = Ball { ballLoc :: Vec2f, ballVel :: Vec2f } deriving(Show)

tickBall :: Ball -> Float -> Ball
tickBall (Ball loc vel) dt = Ball newLoc newVel
    where
        (newVel,newLoc) = runKinematics vzero vel loc dt

data Line v a = Line { lPoint :: v a,lNormal :: v a }

segmentLineIntersect :: Vec2f -> Vec2f -> Line Vec2 Float -> Maybe Float
segmentLineIntersect p1 p2 (Line point normal) = intersect
    where
        intersect = do
            let relP1 = p1 -. point
                relP2 = p2 -. point
            -- If one endpoint is within 90 degrees of the normal and the other
            -- is not, they intersect
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
            return $ negate $ (relP1 `vdot` normal)/(diff `vdot` normal)

data Pong = Pong {
    screenSize :: Vec2f,
    paddleSize :: Vec2f,
    ballSize   :: Vec2f,

    leftPaddle  :: Paddle,
    rightPaddle :: Paddle,
    ball        :: Ball
} deriving(Show)

loadPong :: IO Pong
loadPong = undefined

unloadPong :: Pong -> IO ()
unloadPong = undefined

isPongOver :: Pong -> Bool
isPongOver = undefined

drawPong :: Pong -> IO ()
drawPong = undefined

processPongEvents :: [Event] -> Pong -> Pong
processPongEvents = undefined

tickPong :: Double -> Pong -> Pong
tickPong = undefined

pongTitle :: Pong -> String
pongTitle = undefined

