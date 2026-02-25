---
title: "Tech Interview: Aftertaste"
author: xkollar
tags: Fun, Interview, Haskell
---

<!--
Self note: story about starting simple and finding similarity? Exploring
problem space?
-->

<!-- Dedication: For those "Actually, Fib can be done in constant time" readers -->

Interviews take a lot of energy. In general but
[this one in particular](2026-01-25-tech-interview-ii.html){rel="prev"}.
And getting through it also deserves a little celebration. Whatever
the reason, it is sort of a ritual to go to a nearby food place after.
Raw protein wrapped in carbs, if it can be helped.

The place is quiet, well past the rush of a lunch time, well before the noise
of an evening. Getting ready your linear utensils and squishing totally
reasonable blobs of green stuff onto your units of sustenance, a thought
keeps coming back to you:

<div class="speaker b">
Can we do any better?
</div>

You pull out a writing device of your choice and a generic white-label
wire-bound notebook. You open the notebook on a fresh page and stare at
the empty lines for a bit.

There was a class where someone derived a formula for the Fibonacci
numbers. Or someone mentioned it. And it was exciting. What was it
all about? Something around representing sequences of numbers as
polynomials. Was it called [generating functions](https://en.wikipedia.org/wiki/Generating_function)?

Starting simple is usually a good idea. Maybe encoding a list of all ones?
In polynomials, the various powers of the variable naturally separate their
coefficients. So a sequence $1, 1, 1, 1, 1, 1, \cdots$ could be represented
as a polynomial on $x$ with

$$
f(x) = 1 + x + x^2 + x^3 + x^4 + \cdots
$$

That does not look very useful, but you won't let that discourage you.
This will need some poking around. A fun thing that can be done
with polynomials is to multiply it by the variable, so you try that.

$$
x \cdot f(x) = (0 +\!)\ x + x^2 + x^3 + x^4 + \cdots
$$

Fascinating: This way we shifted the sequence and prepended 0!
This now represents $0, 1, 1, 1, 1, 1, \cdots$. Things are emerging
from murky depths of memory. Let's subtract those...

$$
f(x) - x \cdot f(x) = 1
$$

Now this looks very promising. Suddenly the whole expression becomes
very finite. No need to dance around the infinity with ellipsis…
Just few small steps later we get

$$
f(x) = \frac{1}{1-x}
$$

Somehow, this fraction captures the infinite list of ones.
While possibly not very useful on its own, this feels like an important
piece of a puzzle.

And the whole process reminded you of writing the infinite list
of Fibonacci numbers in Haskell.

```haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

And so you try representing the sequence:

$$
\begin{align}
\overline{\mathrm{fib}}(x) =& 0 + 1x + 1x^2 + 2x^3 + 3x^4 + 5x^5 + \cdots \\
x \cdot \overline{\mathrm{fib}}(x) =& 0 + 0x + 1x^2 + 1x^3 + 2x^4 + 3x^5 + \cdots \\
\overline{\mathrm{fib}}(x) - x \cdot \overline{\mathrm{fib}}(x)
                                   =& 0 + 1x + 0x^2 + 1x^3 + 1x^4 + 2x^5 + \cdots \\
\end{align}
$$

It takes a bit of squinting to realize that the right side is a Fibonacci sequence shifted by 2 positions with extra x
(elements of which you can kinda see in the Haskell implementation!):

<div style="color: orange">
$$
\begin{align}
\overline{\mathrm{fib}}(x) - x \cdot \overline{\mathrm{fib}}(x)
 =& x + x^2 \cdot \overline{\mathrm{fib}}(x) \\
\cdots{} \\
\overline{\mathrm{fib}}(x) =& \frac{x}{1 - x - x^2}
\end{align}
$$
</div>

<div class="speaker b">
This is great, except, what do I do with this? How do I get
back to elements of the sequence? Or even better, to just to
an element on a given position?
</div>

<div class="speaker a">
Try deriving expression for $\sum_{i\in\mathbb{N}_0} \alpha{}^i x^i$.
</div>

<div class="speaker b">
Is that going to help…‽
</div>

You got almost to the end of that sentence before you realized, all at once,
that the interviewer from earlier:

* is sitting right next to you;
* must have been peeking over your shoulder;
* is looking at you with mild interest;
* has mouth freshly stuffed with what looks
  [suspiciously similar to your own food](https://theoldvillage.wordpress.com/2004/01/28/douglas-adams-shares-a-true-story/).

<div class="speaker b">
How long have you…
</div>

<div class="speaker a">
Just try.
</div>

Trying to not get too distracted by what just happened
you repeat the steps, but with extra $\alpha{}$.

$$
\dfrac{1}{1-\alpha{}x} = 1 + \alpha{}x + \alpha^2x^2 + \alpha{}^3x^3 \cdots
$$

That is actually very useful! If we find an appropriately shaped expression,
this formula allows us to find n-th element in the sequence on its own!
And polynomials are easy to sum. That is handy. So is the pattern for
adding two fractions. Now if only we could express this generating
function as a sum of appropriate forms…

$$
\frac{A}{1-\alpha{}x} + \frac{B}{1-\beta{}x}
$$

<details>
<summary>
Using quadratic formula you find the roots of the denominator.
$$
x_{1,2} = - \frac{1 \pm \sqrt{5}}{2}
$$
</summary>

Ugh… Do I even remember quadratic formula? Let me try to remember real quick.

$$
\begin{align}
ax^2 + bx + c &= 0 & /a \\
x^2 + \frac{b}{a}x + \frac{c}{a} &= 0               & -\frac{c}{a} \\
x^2 + \frac{b}{a}x               &= - \frac{c}{a} & + \frac{b}{2a}^2 \\
\end{align}
$$

This way we get the left side to shape of $a^2 + 2ab + b^2$.

$$
\begin{align}
x^2 + \frac{b}{a}x + \frac{b}{2a}^2 &= - \frac{c}{a} + \frac{b}{2a}^2 & \text{undo $(a+b)^2$} \\
(x+\frac{b}{2a})^2 &= - \frac{c}{a} + \frac{b}{2a}^2 & \sqrt{\quad{}} \\
\pm(x+\frac{b}{2a}) &= \frac{\sqrt{b^2 - 4ac}}{2a} \\
x &= \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \\
\end{align}
$$
</details>

<details>
<summary>
From there you reconstruct the individual factors.
$$
1 - x - x^2 = -(x+\frac{1+\sqrt{5}}{2})(x+\frac{1-\sqrt{5}}{2})
$$
</summary>

Roots are only points where the parabola crosses the x-axis, we need to fit
it with multiplicative component, and that is $-1$.
</details>

Next goal is to find $A$ and $B$ such that

$$
\frac{A}{x+\frac{1+\sqrt{5}}{2}} - \frac{B}{x+\frac{1-\sqrt{5}}{2}} = \overline{\mathrm{fib}}(x)
$$

(arbitrarily putting minus on the side of $B$). It takes a bit of a paper
real-estate, but eventually you get to

$$
A,B = -\frac{1\pm{}\sqrt{5}}{2\sqrt{5}}
$$

Even more paper is burned on re-shaping the denominator to the desired
form of $1 - \alpha{}x$.

$$
\frac{-\frac{1}{\sqrt{5}}}{1 -(\frac{-2}{1-\sqrt{5}})x} + \frac{\frac{1}{\sqrt{5}}}{1 -(\frac{-2}{1-\sqrt{5}})x}
$$

And from there n-th element (coefficient for $x^n$) is:

$$
-\frac{1}{\sqrt{5}}(\frac{-2}{1+\sqrt{5}})^n + \frac{1}{\sqrt{5}}(\frac{-2}{1-\sqrt{5}})^n = \frac{(\frac{-2}{1-\sqrt{5}})^n-(\frac{-2}{1+\sqrt{5}})^n}{\sqrt{5}}
$$

<div class="speaker a">
Great work! You can rationalize the denominators in those bases to
make it prettier (and maybe a bit more useful) 👍.
</div>

That sounds reasonable.

$$
\mathrm{fib}(n) = \frac{(\frac{1+\sqrt{5}}{2})^n-(\frac{1-\sqrt{5}}{2})^n}{\sqrt{5}}
$$

<div class="speaker b">
Well, that's a beautiful closed form. And strange one too: all the irrational
numbers, yet it produces natural numbers.

However it still does not allow us to calculate n-th Fibonacci number
in sub-log time! If it was possible there would be some great
improvements to be made here:

* <https://github.com/python/cpython/blob/66bca383bd3b12d21e879d991d77b37a4c638f88/Objects/floatobject.c#L685-L802>
* <https://github.com/python/cpython/blob/66bca383bd3b12d21e879d991d77b37a4c638f88/Objects/longobject.c#L4983-L5261>
</div>

<!-- So actually actually: It can't -->

<div class="speaker a">
Excellent insight. Now how would you turn this into
a function that is able to calculate exact results
on unbounded numbers?
</div>

<div class="speaker b">
<figure>
[![Challenge Accepted](https://media1.tenor.com/m/jZVZa853eNEAAAAd/gif.gif)](https://tenor.com/btXLD.gif)
</figure>

The trick is that we don't need exact value of $\sqrt{5}$. We can
treat it as a special symbol for which $\sqrt{5} \cdot \sqrt{5} = 5$.
Similar-ish to introducing $i$ for $i \cdot i = -1$.

Or we can see it as polynomials factored with $x^2 = 5$.
</div>

<div class="speaker a">
All of it sounds reasonable. But let's let the code speak.
</div>

<div class="speaker b">
Okay, here goes nothing, but I am a bit tired so I'll do
some heavy hand-waving...
```haskell
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

type T = S5 (Rational)

fac :: Integer -> Integer
fac n = numerator x
  where
    -- okay, also by s5 but we know it will be like this
    -- and even the last minus does technically not need
    -- to do the rational part.
    S 0 x = ((1 + s5)/2)^n - ((1 - s5)/2)^n
```
<!-- the "fac" typo is genuine -->

Improvements might be possible but for now the matrix-inspired
version is still more efficient.
</div>

You feel a congratulatory tap on your back.
Well deserved. You feel satisfied. But tired too.
Actually really tired. You blink, look up at the
food-place employee giving you the "we are about to close look".
There is nobody else in here, you are the last customer.

Confused you stand up, stuff all your stuff into the backpack,
say quick thank you and bye and disappear into the night.
