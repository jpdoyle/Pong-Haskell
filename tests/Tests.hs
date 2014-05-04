module Tests where

import Game.Pong.Vector
import Distribution.TestSuite

dumbTest :: TestInstance
dumbTest = TestInstance {
    name = "A dumb test",
    run = return $ Finished $ Fail "Boo hiss",
    tags = [],
    options = []
    setOption _ _ = Left "No Options"
}

tests :: IO [Test]
tests = return [Test $ TestInstance { run = } ]

