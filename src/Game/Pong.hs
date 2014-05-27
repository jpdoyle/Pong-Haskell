module Game.Pong where

import System.Random

import Control.Monad
import Data.Maybe
import Data.Functor
import Game.Event
import Game.Util (sfVec2f,forceCleanup)
import Game.Vector
import Game.Kinematics (runKinematics)
import Control.Monad.SFML (SFML)
import qualified SFML.Graphics as G
import qualified SFML.Window as W
import SFML.Window.Event
import qualified Control.Monad.SFML.Window as WM
import qualified Control.Monad.SFML.Graphics as GM
import SFML.Graphics.Color (black,white)

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

data PongContext = PongContext {
    pcWin :: G.RenderWindow,
    pcFont :: G.Font
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
        (mag,r1) = randomR (0.1,0.1) r -- (0.2,0.4) r

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
        scoreChange = if bounced then Nothing
                      else case drop 4 bounces of
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

pongTitle :: Pong -> String
pongTitle _ = "Pong!"

