---
title: "Tech Interview II"
author: xkollar
tags: Fun, Interview, Haskell
---

<!-- Modified rendering of math. I used to compile
it manually with something like https://temml.org/
and embed resulting MathML. Now trying letting
Pandoc compile it, and the result feels very dense,
I don't think I like it, but maybe it can be CSS-ed? -->

In the [previous installment of the Tech Interview saga](2025-12-25-tech-interview-i.html)
we left off just as things started to get fun, perhaps expecting some quite
heavy chunks of code dropping. Will they materialize? Or perhaps the interview
will switch to Python? (Yeah, you saw the tags, no fooling you!) Keep reading
and find out!

Staring at the art outside the meeting room gave you an uneasy feeling of
discontinuity in time. It feels like you've lost a month doing just that.
But that is impossible, you are still here, doing the interview. There was a
question, right.

You take a deep breath and squeeze the whiteboard marker in your hand,
feeling its weight like a master chef would feel the weight and
the balance of a knife, ready to scribble.

<div class="speaker b">

To get the feel for what we are working with let's look at the first few
powers of the update matrix:

$$
\begin{align}
&
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
&
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
&
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
&
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
&
\cdots{}
\\
\begin{bmatrix}
1 & 0 \\
0 & 1 \\
\end{bmatrix}
&
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
&
\begin{bmatrix}
1 & 1 \\
1 & 2 \\
\end{bmatrix}
&
\begin{bmatrix}
1 & 2 \\
2 & 3 \\
\end{bmatrix}
&
\begin{bmatrix}
2 & 3 \\
3 & 5 \\
\end{bmatrix}
&
\cdots{}
\\
\end{align}
$$

This seems to suggest that each element of the sequence is of form

$$
\begin{bmatrix}
a & b \\
b & a+b \\
\end{bmatrix}
$$

For things to work we need e and f such that

$$
\begin{bmatrix}
a & b \\
b & a+b \\
\end{bmatrix}
\begin{bmatrix}
c & d \\
d & c+d \\
\end{bmatrix}
=
\begin{bmatrix}
e & f \\
f & e+f \\
\end{bmatrix}
$$

Which is indeed the case for

$$
\begin{align}
e & = ac + bd \\
f & = ad + bc + bd \\
\end{align}
$$

This not only shows that we can reduce the original matrices to just tuples,
but it also tells us how to combine them! And based on the way we constructed
the operation it is trivially associative too. Giving us a semigroup. And
while the identity matrix has a corresponding representation in our new
structure making it a monoid, we don't need it now. Cherry on top? Haskell's

[`Semigroup` class has method `stimes`](https://hackage-content.haskell.org/package/ghc-internal-9.1401.0/docs/src/GHC.Internal.Base.html#stimes)
that does what we need, and the default implementation does it the way that we
want!

```haskell
data Fib a = F !a !a deriving Show

un (F x _) = x

instance Num a => Semigroup (Fib a) where
    F a b <> F c d = F (a*c + bd) (a*d+b*c+bd)
      where bd = b*d

fib = un . flip stimes (F 0 1) . succ
```

(Except for strictness, but if you'd want me to
add that I'd just end up copy-pasting that code and
sprinkling in some exclamation marks.)

<!-- And yes, I could have taken the second element and, that way avoid
`succ` and then handle 0 separately, but this feels more elegant.
And possibly removing strictness ftom the second argument
would do as good. -->

</div>

The interviewer takes in a breath to perhaps say something...
but you decide to go on

<div class="speaker b">
I know what you want to say 🤔. This way we do 4 multiplications and 3
additions, but we know that multiplications are more expensive.
Can we do any better? I think we can:

\begin{align}
e & = ac + bd \\
f & = (a+b)(c+d) - ac \\
\end{align}

This way we do 3 multiplications and 4 "additive" operations, except
now we need minus on the underlying numeric types.
</div>

For an unexpectedly shaped moment there is a silence.
You can almost feel it forming at the tip of your tongue.
Also: are you beaming? In any case you feel good. This would
not have been a bad moment to wrap up. Yet by how you
structured the code in your answer... you know more is coming.

<div class="speaker a">
To be honest, I just wanted to say that if we were writing things in Python I
would have made you write the whole thing explicitly, but because you know
about `stimes` and have an idea about its default implementation, I'm willing
to let that one go.
</div>

As you narrow your eyes you can just feel the camera zooming in on
you[...](https://tenor.com/bjU6d.gif?not-sure-fry)

<div class="speaker a">
In any case earlier you correctly identified an issue with Fibonacci numbers:
[they grow too fast](https://images.search.yahoo.com/search/images?p=they+grow+up+so+fast+meme).
For example only the first 94 Fibonacci numbers fit into 64 bits, at which
point one might as well have a static lookup table.
<!-- length . takeWhile (\(a,b) -> toInteger a == b) $ map (\x -> (fib_fun x :: Data.Word.Word64, fib_fun x :: Integer)) [0..] -->
</div>

You nod.

<div class="speaker a">
So what if we wanted to see just last 2 digits of a Fibonacci number?
</div>

<div class="speaker b">
Well, we just need a remainder after division by 100. And because
`mod` is a homomorphism, we can carry out the whole calculation ``(`mod` 100)``.
</div>

<div class="speaker a">
What about last 3 digits in hex?
</div>

<div class="speaker b">
Then that would be ``(`mod` 16^3)``.
</div>

<div class="speaker a">
How is your type magic?
</div>

<div class="speaker b">
Depends. Please don't make me do that without a computer and documentation 🥺.
</div>

<div class="speaker a">
I'm not a monster. Here is a piece of code vaguely inspired by package
[modular-arithmetic](https://hackage.haskell.org/package/modular-arithmetic-2.0.0.3):
```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, type (<=), natVal)


newtype Mod a n = M { unMod :: a }

modulus :: forall n a . (Integral a, KnownNat n, 1 <= n) => a
modulus = fromInteger . natVal $ Proxy @n

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
```

Thoughts?
</div>


<div class="speaker b">
Pretty sweet. Now we can do `fib 10000 :: Mod Int 100` and get something like
`75 (mod 100)`. And it seems like the compiler might be able to be smart
enough to calculate modulus only once 🤔. I quite like it,
but it is usable only if we know modulus at compile time or for playing
on the command line. Also your implementation of negate and minus might
have an issue 👀.
</div>

The interviewer seems to be happy with you being able to read and get
some sense out of this code.

<div class="speaker a">
Yes. Now let us look at the whole modulo situation from a somewhat different
angle: What is the biggest difference between `Natural` and `Mod Natural 100`?
</div>

As you ponder this question you can't help it but feel that it is here not
only to be answered, but also to guide you to some more interesting things.

<div class="speaker b">
One of them is (more) finite. But that means, that the step
endomorphism `\(a,b) -> (b,a+b)` will start looping at some point!
So if we knew where the loop starts and how big it is, we could
calculate the `n`-th Fibonacci number in time completely independent
of the `n` itself! But cycle detection could eat a lot of memory
and testing against all the elements we visited can also add up quickly...
</div>

<div class="speaker a">
Would it help if it was invertible?
</div>

<div class="speaker b">
Well that would make it an isomorphism (automorphism?), which means
there are no "tails" and it generates a cycle, so we know we
would get back to the starting point of `(0,1)`! So no need
to remember all the visited elements!

But is it?
Let's try `(c,d) -> (d-c,c)`. For `(e,f)` after applying
the step we get `(f,e+f)`, and applying the proposed inverse step we get
`(e+f-f,f) = (e,f)`. So it is invertible!
</div>

<div class="speaker a">
Have you heard of Leonardo Pisano?
</div>

<div class="speaker b">
No.
</div>

<div class="speaker a">
He is better known as Fibonacci. And you just discovered something
called [Pisano period](https://en.wikipedia.org/wiki/Pisano_period).
</div>

<div class="speaker b">
How can we make it work with the logarithmic version? Does it even make sense (we are already log, sooo log would need to be bigger than length of loop... still pretty cool...)
</div>

<div class="speaker a">
You ask interesting questions. But this is the end of my time allocated
for this interview so I have to go. Thank you for coming today,
someone will be here with you shortly...
</div>

It happened fast, before you were able to say goodbye, the interviewer is
gone. You are still deep in thought when a different person walks in.

<div class="speaker c">
Hi, sorry for making you wait this long, we had an unexpected
emergency. Are you still available to do the interview now?
</div>

The new person looks at the whiteboard with expression of sudden
realization:

<div class="speaker c">
Not again 😮‍💨 ... Have you... Have someone had a technical
interview round with you just now?
</div>
