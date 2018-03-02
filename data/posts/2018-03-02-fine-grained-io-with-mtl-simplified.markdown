---
title: Fine-grained IO with mtl Simplified
author: xkollar
tags: Haskell, mtl
---

In [previous article](2018-01-25-fine-grained-io-with-mtl.html) I have provided
example of how to do selectively add "impure" actions into mtl monadic stack
without full power of `MonadIO`. This what I have done though was to define
separate newtype wrappers, which is useful if you want to have more cotrol and
flexibility over possible interpreters. Most of the time however, you just want
to have simpler code. If that is what you are aiming for, you can have just one
wrapper newtype and provide all instances for this particular wrapper.

Diff.

```Diff
@@ -27,18 +27,12 @@ class WriteFile m where
     writeFile :: FilePath -> String -> m ()
 
 -- NEWTYPES -------------------------------------
-newtype ReadFileT m a = ReadFile { runReadFile :: m a }
-  deriving (Functor, Applicative, Monad, MonadIO)
-
-newtype WriteFileT m a = WriteFile { runWriteFile :: m a }
+newtype SimpleIORunnerT m a = SimpleIORunner { runSimpleRunner :: m a }
   deriving (Functor, Applicative, Monad, MonadIO)
 
 -- INSTANCES ------------------------------------
-instance MonadTrans ReadFileT where
-    lift = ReadFile
-
-instance MonadTrans WriteFileT where
-    lift = WriteFile
+instance MonadTrans SimpleIORunnerT where
+    lift = SimpleIORunner
 
 instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, ReadFile m) => ReadFile (t m) where
     readFile = lift . readFile
@@ -46,10 +40,10 @@ instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, ReadFile m) => ReadFile (t
 instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, WriteFile m) => WriteFile (t m) where
     writeFile fp = lift . writeFile fp
 
-instance {-# OVERLAPS #-} MonadIO m => ReadFile (ReadFileT m) where
+instance {-# OVERLAPS #-} MonadIO m => ReadFile (SimpleIORunnerT m) where
     readFile = liftIO . IO.readFile
 
-instance {-# OVERLAPS #-} MonadIO m => WriteFile (WriteFileT m) where
+instance {-# OVERLAPS #-} MonadIO m => WriteFile (SimpleIORunnerT m) where
     writeFile fp = liftIO . IO.writeFile fp
 
 -- EXAMPLE --------------------------------------
@@ -68,7 +62,7 @@ combinedAction :: (MonadReader What m, ReadFile m, WriteFile m) => m ()
 combinedAction = readAction >>= writeAction
 
 example1 :: IO ()
-example1 = runWriteFile . runReadFile $ runReaderT combinedAction conf
+example1 = runSimpleRunner $ runReaderT combinedAction conf
   where
     conf = What "/etc/resolv.conf" "/dev/stdout"
```

Full code.

```Haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (Monad, (>>=))
import Data.Function (($), (.), flip, const)
import Data.Functor (Functor, fmap)
import Data.String (String)
import System.IO (FilePath, IO)
import qualified System.IO as IO (readFile, writeFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- CLASSES --------------------------------------
class ReadFile m where
    readFile :: FilePath -> m String

class WriteFile m where
    writeFile :: FilePath -> String -> m ()

-- STACK ----------------------------------------
instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, ReadFile m) => ReadFile (t m) where
    readFile = lift . readFile

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, WriteFile m) => WriteFile (t m) where
    writeFile fp = lift . writeFile fp

-- NEWTYPES and basic INSTANCES -----------------
newtype SimpleIORunnerT m a = SimpleIORunner { runSimpleRunner :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SimpleIORunnerT where
    lift = SimpleIORunner

-- IMPLEMENTATIONS ------------------------------
instance {-# OVERLAPS #-} MonadIO m => ReadFile (SimpleIORunnerT m) where
    readFile = liftIO . IO.readFile

instance {-# OVERLAPS #-} MonadIO m => WriteFile (SimpleIORunnerT m) where
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
main = runSimpleRunner $ runReaderT combinedAction conf
  where
    conf = What "/etc/resolv.conf" "/dev/stdout"
```
