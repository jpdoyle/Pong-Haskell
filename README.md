PONG
====

It's the very first video game ever, now on your PC!

Installing
----------

This uses [Cabal](http://www.haskell.org/cabal/download.html) to build
and run. Once you have Cabal, just run this in the root of the
project:

    $ cabal configure
    $ cabal run

Playing
-------

The game starts paused (\<Space\> toggles this). The left paddle is
controlled with W/S, the right with Up/Down. The ball bounces
differently depending on where it hits the paddle, and speeds up with
each bounce. There currently is no winning score, so play forever or
until you're bored!

