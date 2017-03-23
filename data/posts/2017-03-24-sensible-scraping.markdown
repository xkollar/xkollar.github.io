---
title: Sensible Scraping
author: xkollar
tags: Haskell
---
Whether you are developing tool for accessing publicly provided
API or accessing data in less official way (e.g. scraping)
chances are service you are using will not appreciate
larger amounts of requests that development of such tools
tend to require.

And to be honest, repeated identical requests are unpleasant
even from the developer point of view as they unnecessarily
stretch experimentation loops.

Long story short, what one wants is caching. I remember writing
something like that as a Python decorator, so I gave it a try also
in Haskell.


```Haskell
module Cached (cached) where

import Data.Monoid ((<>))

import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)


toHex :: BS.ByteString -> String
toHex bytes = BS.unpack bytes >>= printf "%02x"

cached
    :: FilePath
    -> (String -> IO BSL.ByteString)
    -> String
    -> IO BSL.ByteString
cached cacheDir action arg = do
    let cacheFile = cacheDir </> (toHex . hash $ BS8.pack arg)
    hit <- doesFileExist cacheFile
    if hit
        then do
            putStrLn $ "Cached hit: " <> arg
            BSL.readFile cacheFile
        else do
            putStrLn $ "Cached miss: " <> arg
            createDirectoryIfMissing True cacheDir
            writeFile (cacheFile <> ".arg") arg
            c <- action arg
            BSL.writeFile cacheFile c
            pure c
```

(More generic version with `Show`/`Read` or some other generic
serialization/deserialization certainly is possible but I went
with specific types because of my particular use case.)

Having caching in place, one might still encounter some issues.
Some sites lay down rules for using their API in terms of how many
requests you can make per time unit to avoid being blacklisted.

Again very simple wrapper.

```Haskell
module Delayed (delayed) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception.Base (evaluate)
import Data.Monoid ((<>))
import System.IO.Unsafe (unsafePerformIO)

import Data.Time
    (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)


nextAllowed :: MVar UTCTime
nextAllowed = unsafePerformIO $ getCurrentTime >>= newMVar

microSecondsRemains :: UTCTime -> UTCTime -> Int
microSecondsRemains t1 t2 = toMicroSeconds $ diffUTCTime t1 t2 * 1000000
  where
    toMicroSeconds = max 0 . round . toRational

delayed :: NominalDiffTime -> IO a -> IO a
delayed dt a = do
    waitTime <- microSecondsRemains <$> takeMVar nextAllowed <*> getCurrentTime
    putStrLn $ "Waiting " <> show waitTime
    threadDelay $ waitTime
    res <- a >>= evaluate
    getCurrentTime >>= putMVar nextAllowed . addUTCTime dt
    pure res
```
Using `unsafePerformIO` is probably not the best practice, but the `nextAllowed`
is kept hidden (not exported) and it makes use of the module much simpler.

Exercises for patient reader
----------------------------

* Make generic version of `cached` that would use some
  serialization/deserialization mechanism.
* Would `delayed` work for multi-threaded scraper?
  (Does it need to, considering our constraints?)
* Implement wrapper that would allow for bursts of requests, but without
  exceeding `n` in a given time unit. (Token bucket, leaky bucket, or
  whatever.)
* Introduce randomness to the time.
* Make transformer version using `MonadIO` constraint.
* Make `freer-effects` version and instead of `putStrLn` use `trace`.
