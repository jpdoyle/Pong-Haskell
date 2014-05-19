module Game.Event where

import qualified Graphics.UI.SDL as S

-- In case I come up with more event types later
data Event = SDLEvent S.Event

allAvailableEvents :: IO [Event]
allAvailableEvents = do
    e <- S.pollEvent
    case e of
        S.NoEvent -> return []
        _         -> do
                        es <- allAvailableEvents
                        return (SDLEvent e:es)

