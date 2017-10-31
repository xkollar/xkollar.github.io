---
title: How to Show a Constructor
author: xkollar
tags: Haskell
---
How to show a (toplevel) constructor in a generic way? (Pun intended.)

Not really a complicated thing but I thought that it might be nice, simple, and
yet valuable example for generic programming in Haskell using `GHC.Generics`.

Here is the code.

```Haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Data.Generics.ShowConstructor
    ( showConstr
    ) where

import Data.Function ((.))
import Data.String (String)
import GHC.Generics
       ((:+:)(L1, R1), C, Constructor, Generic, M1(M1), Rep, conName,
        from)


class ShowConstr f where
    gShowConstr :: f p -> String

instance (ShowConstr a, ShowConstr b) => ShowConstr (a :+: b) where
    gShowConstr (L1 x) = gShowConstr x
    gShowConstr (R1 x) = gShowConstr x
    {-# INLINE gShowConstr #-}

instance {-# OVERLAPPABLE #-} ShowConstr a => ShowConstr (M1 i c a) where
    gShowConstr (M1 x) = gShowConstr x
    {-# INLINE gShowConstr #-}

instance Constructor c => ShowConstr (M1 C c a) where
    gShowConstr = conName
    {-# INLINE gShowConstr #-}

showConstr :: (Generic a, ShowConstr (Rep a)) => a -> String
showConstr = gShowConstr . from
{-# INLINE showConstr #-}
```

Now let's talk about it a little.

Usual questions I get from people dipping their toes to `Generics` for the
firs time are:

* Why do I need to create the class?
* Why are there so many instances?
* What are these types for which there are instance for?
* Why there is `ShowConstr (Rep a)` constraint in type of `showConstr`?

Class `Generics` has not only associated functions
`from :: a -> Rep a x`{.Haskell} and `to :: Rep a x -> a`{.Haskell} (ignore `x`
for now), but also type (family) `Rep a`{.Haskell}, representation of type `a`.
This type is built out of only very few other types (see [Generics documentation on
Hackage](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)).
(Couple more than I have instances for, but I do not need them in my case.)

To be able to traverse this type, we use Haskell-s ad-hoc polymorphism mechanism,
type classes. Take for example Instance `ShowConstr (M1 i c a)`{.Haskell}.
We do not know type of `x` in `M1 x`, and yet we want to process it.
And type classes are Haskell-s answer for this sort of problem `:-)`.

And just to wrap things up: if you have instances for
`Data.Data.Data`{.Haskell}, you can indeed go for slightly simpler solution.

```Haskell
import Data.Data (Data, toConstr)

showConstr :: Data a => a -> String
showConstr = show . toConstr
```

Or yet even (in some sense) simpler, requiring only `Text.Show.Show`{.Haskell}
(but with some trouble-causing corner cases (Lists, Strings, and more generally
types with custom Show instance, …)).

```Haskell
showConstr :: Show a => a -> String
showConstr = takeWhile (not . isSpace) . show
```
