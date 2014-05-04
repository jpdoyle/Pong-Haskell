module Game.Pong where

import Game.Event (Event)

data Pong = Pong

loadPong :: IO Pong
loadPong = undefined

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

