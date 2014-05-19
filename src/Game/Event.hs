module Game.Event where

import Control.Monad.SFML
import qualified SFML.Window as W
import Control.Monad.IO.Class

data Event = SFEvent W.SFEvent

allAvailableEvents :: MonadIO io => W.SFWindow w => w -> io [Event]
allAvailableEvents w = liftIO $ do
    e <- W.pollEvent w
    case e of
        Just ev -> do
                    es <- allAvailableEvents w
                    return (SFEvent ev:es)
        Nothing -> return []

