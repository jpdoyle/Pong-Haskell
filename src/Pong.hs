-- module Game.Pong where
module Main where

import Control.Monad.SFML (runSFML)

import Game.Pong.Run (runPong)

main :: IO ()
main = runSFML runPong

