import Data.Ratio

data S5 a = S !a !a deriving Show

s5 :: Num a => S5 a
s5 = S 0 1

instance Num a => Num (S5 a) where
    S a b + S c d = S (a+c) (b+d)
    S a b - S c d = S (a-c) (b-d)
    S a b * S c d = S (a*c+5*b*d) (a*d+b*c)
    fromInteger a = S (fromInteger a) 0
    negate (S a b) = S (negate a) (negate b)
    abs _ = error "leave me alone"
    signum _ = error "meh"

instance (Eq a, Fractional a) => Fractional (S5 a) where
    fromRational a = S (fromRational a) 0
    S a b / S c 0 = S (a/c) (b/c) -- we only divide by 2

type T = S5 Rational

fac :: Integer -> Integer
fac n = numerator x
  where
    -- okay, also by s5 but we know it will be like this
    -- and even the last minus does technically not need
    -- to do the rational part.
    S 0 x = ((1 + s5)/2)^n - ((1 - s5)/2)^n

main = print $ map fac [0..10]
