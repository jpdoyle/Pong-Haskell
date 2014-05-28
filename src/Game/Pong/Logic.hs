module Game.Pong.Logic where

import System.Random

import Data.Maybe
import Game.Event
import Game.Vector

import Game.Pong.Core

shiftLineToEdgeOnAxis :: Vec2f -> Vec2f
                               -> (Vec2f,Vec2f) -> (Vec2f,Vec2f)
shiftLineToEdgeOnAxis axis size (cStart,cEnd) = (start,end)
    where
        start = cStart +. offset
        end = cEnd +. offset
        offset = factor *. diff
        factor = axisSize / axisDiff
        axisDiff = vmag $ diff `vproject` axis
        axisSize = vmag $ (0.5*.size) `vproject` axis
        diff = cEnd -. cStart

restrainTo60deg :: Vec2f -> Vec2f
restrainTo60deg v = if tooVert then corrected else v
    where
        -- tan(60 deg) = sqrt(3) = 0.8660254
        tooVert = abs (tangent v) > 0.8660254
        corrected = vmag v *. (vsgn v .*. Vec2 0.5 0.8660254)
        tangent v = v2y v / v2x v
        vsgn (Vec2 x y) = Vec2 (signum x) (signum y)

checkGoal :: Vec2f -> Vec2f -> Vec2f -> (Paddle,Paddle) -> Ball
                   -> Ball -> (Ball,Maybe Float,Maybe Side)
checkGoal ssize paddleSize ballSize (leftPaddle,rightPaddle)
          startBall endBall
        = (newBall,bounceFactor,scoreChange)
    where

        newBall
            | scored    = Ball (0.5 *. ssize) vzero
            | bounced   = Ball bounceLoc bounceVel -- Make this finish
                                                   -- the frame's
                                                   -- interpolation?
            | otherwise = endBall
        scored = case scoreChange of
                    Nothing -> False
                    _       -> True
        scoreChange
            | v2x (ballLoc endBall) - v2x ballSize
               < v2x (paddleLoc leftPaddle) = Just RightSide
            | v2x (ballLoc endBall) + v2x ballSize
               > v2x (paddleLoc rightPaddle) = Just LeftSide
            | bounced   = Nothing
            | otherwise = case drop 4 bounces of
                [Nothing,Nothing] -> Nothing
                [Nothing,_]       -> Just LeftSide
                _                 -> Just RightSide
        -- prevents bounces from being too vertical
        bounceVel = restrainTo60deg bounceVelInit
        bounceVelInit = (1.1*vmag (ballVel startBall))
                         *.vnorm (bounceLoc -. paddleLoc bouncePaddle)
        bounceLoc = maybe (ballLoc endBall)
                          (interp (ballLoc startBall)
                                  (ballLoc endBall))
                          bounceFactor
        bounceFactor = listToMaybe $ catMaybes
                                   $ zipWith check bounceds factors

        check b m = if b then m else Nothing
        firstJust = foldr $ flip fromMaybe
        bouncePaddle = snd $ head $ filter fst
                           $ zip bounceds paddles
        bounced = or bounceds
        bounceds = map (==Just True) bounces
        bounces = zipWith (fmap . onPaddle) paddles intLocs
        paddles = [leftPaddle,rightPaddle,
                   leftPaddle,rightPaddle,
                   leftPaddle,rightPaddle]
        onPaddle paddle (Vec2 x y) =
            let py = v2y $ paddleLoc paddle
                h  = v2y paddleSize
                bs = v2y ballSize in
            y + 0.5*bs >= py - h/2 && y - 0.5*bs <= py + h/2
        intLocs = map (fmap $ interp (ballLoc startBall) (ballLoc endBall))
                      factors

        interp start end t = start +. t *. (end -. start)
        factors = zipWith ($) [edgeIntersect,edgeIntersect,
                               onLeftFact,onRightFact,
                               centerIntersect,centerIntersect]
                              [leftPaddleLine,rightPaddleLine,
                               undefined,undefined,
                               leftLine,rightLine]
        onLeftFact = let (Vec2 bx _) = ballLoc endBall -. 0.5*.ballSize
                         (Vec2 bvx _) = ballVel startBall
                         (Vec2 px _) = paddleLoc leftPaddle
                                       +. paddleHalfSize in
                     const (if bvx < 0 && bx < px then Just 1 else Nothing)
        onRightFact = let (Vec2 bx _) = ballLoc endBall +. 0.5*.ballSize
                          (Vec2 bvx _) = ballVel startBall
                          (Vec2 px _) = paddleLoc rightPaddle
                                        -. paddleHalfSize in
                      const (if bvx > 0 && bx > px then Just 1 else Nothing)
        centerIntersect = segLineIntersect (ballLoc startBall)
                                           (ballLoc endBall)
        edgeIntersect = segLineIntersect edgeStart edgeEnd
        (edgeStart,edgeEnd) = shiftLineToEdgeOnAxis
                                    (Vec2 1 0)
                                    ballSize
                                    (ballLoc startBall,
                                     ballLoc endBall)
        ballOffset = abs (ballSize `vdot` ballChange) *. ballChange
        -- ballsgn = negate $ signum $ pBallSize p `vdot` ballChange
        ballChange = ballLoc endBall -. ballLoc startBall
        leftPaddleLine = Line (paddleLoc leftPaddle
                               +. Vec2 (v2x paddleHalfSize) 0)
                              (Vec2 1 0)
        leftLine = Line (paddleLoc leftPaddle) (Vec2 1 0)
        rightPaddleLine = Line (paddleLoc rightPaddle
                                -. Vec2 (v2x paddleHalfSize) 0)
                               (Vec2 (-1) 0)
        rightLine = Line (paddleLoc rightPaddle) (Vec2 (-1) 0)
        paddleHalfSize = 0.5 *. paddleSize

tickPong :: Float -> Pong -> Pong
tickPong _ p@Pong{pRunning=False} = p
tickPong dt p@Pong{pScreenSize=ssize,
                   pPaddleSize=psize,
                   pBallSize=bsize,
                   pBall=ball,
                   pLeftPaddle=lPaddle,
                   pRightPaddle=rPaddle,
                   pLeftDir=ldir,
                   pRightDir=rdir,
                   pLeftScore=lscore,
                   pRightScore=rscore,
                   pRandom=rand} = p{pBall=newBall,
                                     pLeftPaddle=newLPaddle,
                                     pRightPaddle=newRPaddle,
                                     pLeftScore=newLScore,
                                     pRightScore=newRScore,
                                     pRandom=newRand,
                                     pFactor = fact}
    where
        (newBall,newRand) = case score of
            Nothing -> (scoredBall,rand)
            _ -> let (vel,r) = randBallVel ssize rand in
                 (scoredBall{ballVel=vel},r)
        newLScore = if score == Just LeftSide  then lscore+1
                                               else lscore
        newRScore = if score == Just RightSide then rscore+1
                                               else rscore
        (scoredBall,fact,score) = checkGoal ssize psize bsize
                                         (lPaddle,rPaddle) ball ball1
        ball1 = boundBall vzero ssize (v2x bsize) $ tickBall dt ball
        newLPaddle = boundPaddle 0 h (v2y psize)
                     $ tickPaddle ldir h dt lPaddle
        newRPaddle = boundPaddle 0 h (v2y psize)
                     $ tickPaddle rdir h dt rPaddle
        h = v2y ssize

