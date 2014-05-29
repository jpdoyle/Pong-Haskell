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

You will need [SFML-Haskell](https://github.com/SFML-haskell/SFML) and
[SFML-Control](https://github.com/SFML-haskell/SFML-control) to
compile this, and the [CSFML](http://www.sfml-dev.org/download/csfml/)
library to link and run it. Cabal can install any other necessary
libraries.

Playing
-------

The game starts paused (\<Space\> toggles this). The left paddle is
controlled with W/S, the right with Up/Down. The ball bounces
differently depending on where it hits the paddle, and speeds up with
each bounce. There currently is no winning score, so play forever or
until you're bored!

Screenshots
-----------

![1](/../screenshots/screenshots/1.png)
![4](/../screenshots/screenshots/4.png)
![6](/../screenshots/screenshots/6.png)
![9](/../screenshots/screenshots/9.png)

