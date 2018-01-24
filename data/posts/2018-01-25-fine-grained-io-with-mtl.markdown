---
title: Fine-grained IO with mtl
author: xkollar
tags: Haskell
---

The [mtl](https://hackage.haskell.org/package/mtl) library is very convenient
to work with. What many people do not like about it is that you do not have
suffucient granularity over `IO`, like solutions build on ideas of extensible
effects (like
[freer-effects](https://github.com/IxpertaSolutions/freer-effects)). Or is
there?

What will I show you is how to get such granularity with just "ordinary"
mtl-style transformer stack.

Lets say we would like to have two separate constraints: one allowing our
component to read files, and another to write files. We would like to be able
to use it in any stack (which implies in different types). Haskell's answer for
ad-hoc polymorphism are type classes, so we create two of them, with
appropriate names and methods (`ReadFile` and `WriteFile`).

Other thing we will use is newtype wrappers for building transformers stacks
(`ReadFileT` and `WriteFileT`). `GeneralizedNewtypeDeriving` will
make things simpler for us there.

Then to be able to position our stacks arbitrarily, we will make instances
for them for `MonadTrans` class.

Once we have that we need to provide instances for our classes (constraints
for reading and writing files), both for whole transformer stack and for
particular newtype wrappers. (I used overlapping instances. Maybe there is a
way to do it without those?)

And we are done. Included simple example that demonstrates use of the code,
demonstrating also integration with `MonadReader` from `mtl`.

Feel free to play with it and try to sneak in some other IO for example to
`writeAction`, I dare you `;-)`.

```Haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Applicative (Applicative)
import Control.Monad (Monad, (>>=))
import Data.Function (($), (.), flip)
import Data.Functor (Functor)
import Data.String (String)
import System.IO (FilePath, IO)
import qualified System.IO as IO (readFile, writeFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)


-- CLASSES --------------------------------------
class ReadFile m where
    readFile :: FilePath -> m String

class WriteFile m where
    writeFile :: FilePath -> String -> m ()

-- NEWTYPES--------------------------------------
newtype ReadFileT m a = ReadFile { runReadFile :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype WriteFileT m a = WriteFile { runWriteFile :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- INSTANCES ------------------------------------
instance MonadTrans ReadFileT where
    lift = ReadFile

instance MonadTrans WriteFileT where
    lift = WriteFile

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, ReadFile m) => ReadFile (t m) where
    readFile = lift . readFile

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, WriteFile m) => WriteFile (t m) where
    writeFile fp = lift . writeFile fp

instance {-# OVERLAPS #-} MonadIO m => ReadFile (ReadFileT m) where
    readFile = liftIO . IO.readFile

instance {-# OVERLAPS #-} MonadIO m => WriteFile (WriteFileT m) where
    writeFile fp = liftIO . IO.writeFile fp

-- EXAMPLE --------------------------------------
data What = What
    { from :: FilePath
    , to :: FilePath
    }

readAction :: (MonadReader What m, ReadFile m) => m String
readAction = asks from >>= readFile

writeAction :: (MonadReader What m, WriteFile m) => String -> m ()
writeAction c = asks to >>= flip writeFile c

combinedAction :: (MonadReader What m, ReadFile m, WriteFile m) => m ()
combinedAction = readAction >>= writeAction

main :: IO ()
main = runWriteFile . runReadFile $ runReaderT combinedAction conf
  where
    conf = What "/etc/resolv.conf" "/dev/stdout"
```

And you can go much crazier than this. For example add tags to newtypes, that
would indicate, what files you can actually read and write… I leave this
as an exercise for patient reader though `;-)`.
