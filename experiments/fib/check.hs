{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
import Data.Bool ((||))
import Numeric.Natural (Natural)
import Data.Function (on)
import System.IO.Unsafe

import Data.Semigroup (Semigroup((<>), stimes))
import Data.Proxy (Proxy(Proxy))

-- import GHC.TypeLits hiding (Mod)
import GHC.TypeLits (KnownNat, type (<=), natVal)

canPack :: Natural -> Natural -> Bool
canPack = (||) `on` even

fib_1 = (!!) fibs
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fib_2 = go 0 1
  where
    go a _ 0 = a
    go a b n = go b (a+b) (n-1)

fib_3 = go 0 1
  where
    go !a !_ 0 = a
    go a b n = go b (a+b) (n-1)

data Fib a = F !a !a deriving Show

un (F x _) = x

instance Num a => Semigroup (Fib a) where
    -- F a b <> F c d = F (ac + bd) (stuff-ac)
    --   where
    --     ac = a*c
    --     bd = b*d
    --     stuff = (a+b)*(c+d)
    F a b <> F c d = F (a*c + bd) (a*d+b*c+bd)
      where bd = b*d

fib_fun :: (Integral a, Num b) => a -> b
fib_fun = un . flip stimes (F 0 1) . succ

newtype Mod a n = M a
unMod (M a) = a

-- {-# NOINLINE modulus #-}
modulus :: forall n a . (Integral a, KnownNat n, 1 <= n) => a
-- modulus = fromInteger . natVal $ Proxy @n
modulus = fromInteger . called "modulus" natVal $ Proxy @n

-- instance (Show a, KnownNat n) => Show (Mod a n) where
--     showsPrec a n@(M x) = showParen (a>9) $ shows x . (' ':) . showParen True (("mod "<>) . shows (natVal n))
instance (Show a, KnownNat n) => Show (Mod a n) where
    showsPrec a n@(M x) = showParen (a>9) $
        shows x .
        (' ':) .
        showParen True (("mod "<>) . shows (natVal n))

instance (Integral a, KnownNat n, 1 <= n) => Num (Mod a n) where
  M x + M y = M $ (x + y) `mod` modulus @n
  M x - M y = M $ (x - y) `mod` modulus @n
  M x * M y = M $ (x * y) `mod` modulus @n
  negate (M x) = M $ (modulus @n) - x
  abs x = x
  signum (M x) = M $ signum x
  fromInteger x = M . fromInteger $ x `mod` modulus @n


type Mod10 = Mod Int 10
type Mod100 = Mod Int 100
type Mod1000 = Mod Int 1000

called label f x = unsafePerformIO $ do
    putStrLn label
    pure $ f x

main = do
    print (fib_fun 0 :: Mod Int 10)
    print (fib_fun 0 :: Mod Int 10)
    print (fib_fun 8 :: Mod Int 10)
    print (fib_fun 8 :: Mod Int 100)
    print (fib_fun 100 :: Mod Int 100)
    print (fib_fun 1000 :: Mod Int 1000)
    print (fib_fun 10000 :: Mod Int 1000)
