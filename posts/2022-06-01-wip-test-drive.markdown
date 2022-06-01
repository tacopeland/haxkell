---
title: WIP Test Drive
tags: write-ups, crypto
og-description: Test drive of my WIP Haskell library for solving CTF questions.
---

While working on my Haskell library, haskell-ctf-crypto, I have spent more time
filling my todo-list than actually working on it. So, I have decided to make
use of this library to solve crypto CTF challenges, in order to shed light on
the shortcomings of this library and provide direction for further development.

<!--more-->

# Stack

So, we're off to solve some CTF challenges, right? Wrong.

Here is where I hit my first snag: it is extremely painful to run a Haskell
script using a library not on Hackage. To run a Stack script, the "accepted"
method is to preface a Haskell script with a POSIX shebang pointing to Stack,
with arguments specifying the resolver and packages to import. I could not do
this, because my crypto library is not yet on the Hackage database. So, I had to
forego Stack scripts altogether and simply create a new stack project for each
CTF challenge, which is extremely annoying. To alleviate some of the annoyance
and inelegance, I have created a minimal project template to use with
`stack new` when creating a new challenge folder. The template is as follows:
## script.hsfiles
```
{-# START_FILE {{name}}.cabal #-}
name: {{name}}
version: 0
build-type: Simple
cabal-version: >=1.10

executable {{name}}
  main-is: {{name}}.hs
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base ==4.*
    , haskell-ctf-crypto
  default-language: Haskell2010

{-# START_FILE stack.yaml #-}
resolver: lts-19.9
packages:
- .
extra-deps:
- github: tacopeland/haskell-ctf-crypto
  commit: 91efa7220fe0ea35ef8d0d85fe4634073db3f3d8
- github: gilith/factor
  commit: 482b2eb792ab63a51f2c3a514cdfb0b76f9205a5

{-# START_FILE {{name}}.hs #-}
main :: IO ()
main = do
    print "It works!"

```

Even with this, I have 3 extra files plus 1 hidden folder `.stack-work` in
every challenge folder.

From what I gather, the need to get packages exclusively from the internet
and not from the local filesystem is for the much-desired property of
"reproducability", which means something along the lines of "either EVERYBODY
can run this script, or nobody can". While this means anybody using Stack
can run this script on any system Stack supports with just a `stack run`
after downloading the folder, it creates extra files and makes the directory
slightly messy. You win some, you lose some.


# [AngstromCTF 2021](https://2021.angstromctf.com)

I decided to start with AngstromCTF 2021. This is because it's relatively easy,
still accessible online, and has quality challenges. I will start from the
easiest challenges and proceed to the harder ones.

## Relatively Simple Algorithm (40 points)

This is an extremely easy RSA challenge, where you are given the modulus fully
factored, along with the public exponent and ciphertext, and you simply need
to decrypt the ciphertext after calculating the private key from the factored
modulus. Here is the script:
```haskell
import Crypto.Util.Encoding (integerToBytes)
import Crypto.PublicKey.RSA (constructFacE, decrypt)

main :: IO ()
main = let
    n = 113138904645172037883970365829067951997230612719077573521906183509830180342554841790268134999423971247602095979484887092205889453631416247856139838680189062511282674134361726455828113825651055263796576482555849771303361415911103661873954509376979834006775895197929252775133737380642752081153063469135950168223
    p = 11556895667671057477200219387242513875610589005594481832449286005570409920461121505578566298354611080750154513073654150580136639937876904687126793459819369
    q = 9789731420840260962289569924638041579833494812169162102854947552459243338614590024836083625245719375467053459789947717068410632082598060778090631475194567
    e = 65537
    c = 108644851584756918977851425216398363307810002101894230112870917234519516101802838576315116490794790271121303531868519534061050530562981420826020638383979983010271660175506402389504477695184339442431370630019572693659580322499801215041535132565595864123113626239232420183378765229045037108065155299178074809432
    key = constructFacE [p,q] e
    Just m = decrypt c key
  in
    print (integerToBytes m)
```

This was extremely easy, for a few reasons:
- The RSA parameters given in a text file could be directly copied into the
    script, then indented.
- The `constructFacE :: [Integer] -> Integer -> RSAKey` function constructs an
    RSA private key given factors and a public exponent, taking care of the
    calculation of $$\phi=(p-1)(q-1)$$ and $$e$$.
