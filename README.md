*scoutess* is a WIP build bot software for cabal-powered Haskell projects. Please note that it is not usable yet.

For a post containing example usage, see [this post](http://projectscoutess.blogspot.co.uk/2012/08/mission-report.html).

If you have questions, if you would like to discuss that project with us or contribute to it, feel free to email alpmestan@gmail.com or join #scoutess on irc.freenode.net.

Scoutess assumes you have cabal with sandboxes integrated. In addition, you'll want to have a clean-room install of ghc available. For the included MainNew, I downloaded the ghc binary from http://www.haskell.org/ghc and did:

  $ ./configure --prefix=/opt/ghc-7.6.3
  $ make
  $ sudo make install

Scoutess then uses this clean room install + sandboxes so that:

 1. packages in your normal GHC environment are not visible
 2. building packages with scoutess does not affect the cleanroom install

