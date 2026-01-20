---
title: "Tech Interview I"
author: xkollar
tags: Fun, Interview, Haskell
---

Today is the day. After sitting for a few minutes
in a comfy chair in a lobby of an unassuming office building
with serene art on the wall and pretending you are
interested in the magazines available, you are called into
what feels like a for-two-people-oversized meeting room
where a smiling person with an outfit as if generated
from a prompt "unassuming geek with glasses" is ready for you.

<div class="speaker a">
Hello and welcome. I hope your wait was a pleasant one.
Just to make sure, you are here for an interview for a
position with ███ █████ where you'd be doing computer stuff.
Is that what you expect?
</div>

...the person says, gesturing towards a chair where there
is already some paper, a few pens and whiteboard markers,
a glass, and a bottle of water.

<div class="speaker b">
Hi, nice to meet you, and yes, this is my expectation for today's meeting.
</div>

As you sit down, the person continues:

<div class="speaker a">
Session with me will be somewhat technical, interactive, and hopefully fun.

I've been with the company for *time period* and I'm part of
a *cryptic and not much saying name of a team possibly
referencing some niche-culture thing* where I work on
*some technical stuff*. We mostly use *programming language*
and *storage backend* on top of *operating system* running in
*deployment technology* that runs in *some cloud stuff or on-prem
or something*, but also use *list of almost every other language,
storage technology, way of deployment, operating system, and whatnot*.
</div>

<div class="speaker b">
Nice. I have heard some of those words.
</div>

<div class="speaker a">
Haha. Anyway. Let's get to the fun part. Given an area on a unit-square grid
and some [polyominoes](https://en.wikipedia.org/wiki/Polyomino), how would you
go about determining whether it is possible to use these (possibly with
repetition) to fill the given area.
</div>

After quick pause to wonder about how the interviewer managed to include a
hyperlink in his speech you snap back to your role of an interviewee and
start thinking: "The question is a bit vague, but I know what it is
asking. Or at least I think I do. Perhaps I should ask some clarifying
questions?"

After a short moment you say:

<div class="speaker b">
Well, In the general case I would just brute-force it. Start with one square
in the area, and iterate over the shapes trying to place them there
(and all rotations x flips if that is desired too), and then recursively...
</div>

<div class="speaker a">
Sounds good, what would be the complexity of this algorithm?
</div>

You are a bit startled by the interruption but decide it is okay.
Perhaps it is part of the test? To be able to quickly adapt and
to think on your feet is important...

<div class="speaker b">
Exponential in terms of ...
</div>

But again before you manage to finish your answer you get interrupted.

<div class="speaker a">
Sounds like you are saying it is a hard problem. Let me make it simpler.
</div>

You want to say something, but the only thing that comes out is

<div class="speaker b">
...
</div>

<div class="speaker a">
Let's make it a rectangular area with sides `M` and `N` and you need to tile
it with dominoes.
</div>

Hoping that at least this time you'll get to finish your thought you start.

<div class="speaker b">
Every domino fills two squares. So I definitely can't do rectangle with odd
area. Now the question is, whether I can do all possible even areas. Even area
of `MxN` has even `M` or `N`. Without loss of generality say it is `M`. Then
we have `N` lines (`N` can be also even), that has `M = 2*X` unit squares,
which we can trivially tile with `X` dominoes. Therefore: we can tile
rectangular area of `MxN` unit squares with dominoes if and only if at least
one of `M, N` is an even number.
</div>

<div class="speaker a">
You mention Haskell on your CV. Can you write your solution in Haskell?
</div>

<div class="speaker b">
```haskell
import Data.Bool ((||))
import Data.Function (on)
import Numeric.Natural (Natural)

canPack :: Natural -> Natural -> Bool
canPack = (||) `on` even
```
</div>

You surprised yourself with your ability to speak with syntax highlighting but
decide to just roll with it like nothing happened. At least you demonstrated
that Haskell on your CV is not just decorative, and with chilled point-free
style at that!

<div class="speaker a">
So you are saying that for example rectangle `2xN` can be tiled
with dominoes for any `N`?
</div>

<div class="speaker b">
Yes.
</div>

<div class="speaker a">
In how many ways?
</div>

You feel this is a good moment for a clarifying question.

<div class="speaker b">
Excuse me?
</div>


<div class="speaker a">
How many unique domino-tilings of `2xN` rectangular area are there?
</div>

For a bit you think about whether some off-by-one arrangements
are possible and how to make sure you won't count same cases
multiple times, when you decide to try to build things on that
brute-force idea from earlier and see where it goes from there...

You look with a question in your eyes at the whiteboard markers
and then into the eyes of the interviewer. The interviewer
only briefly breaks the eye contact to look at the whiteboard
giving you the answer.

<div class="speaker b">
Let's have a grid of `2xN`. Let's break it down to several cases.

<svg viewBox="0 0 120 70" fill="white">
  <style>
  text {
    font: 5px sans-serif;
    fill: black;
  }
  </style>
  <defs>
    <pattern id="grid" width="10" height="10" patternUnits="userSpaceOnUse">
      <rect width="10" height="10" fill="none" stroke="black" stroke-width="0.5"/>
    </pattern>
  </defs>
  <!-- Background -->
  <rect x="0" y="0" width="150" height="100" fill="white" />
  <!-- N=0 -->
  <rect x="15" y="10" width="0.001" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
  <text x="15" y="8" text-anchor="middle">N=0</text>
  <!-- N=1 -->
  <rect x="10" y="40" width="10" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
  <text x="15" y="38" text-anchor="middle">N=1</text>
  <!-- General case -->
  <g transform="translate(40 10)">
    <rect x="0" y="0" width="40" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
    <g transform="translate(40 0)">
      <path stroke-dasharray="2,2" d="M0  0 l10 0" stroke="black" stroke-width="0.5" />
      <path stroke-dasharray="2,2" d="M0 10 l10 0" stroke="black" stroke-width="0.5" />
      <path stroke-dasharray="2,2" d="M0 20 l10 0" stroke="black" stroke-width="0.5" />
    </g>
    <rect x="50" y="0" width="10" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
    <rect x="1" y="1" width="8" height="18" fill="#f999" stroke="#f99c" stroke-width="1"/>
  </g>
  <text x="70" y="8" text-anchor="middle">N</text>
  <g transform="translate(40 40)">
    <rect x="0" y="0" width="40" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
    <g transform="translate(40 0)">
      <path stroke-dasharray="2,2" d="M0  0 l10 0" stroke="black" stroke-width="0.5" />
      <path stroke-dasharray="2,2" d="M0 10 l10 0" stroke="black" stroke-width="0.5" />
      <path stroke-dasharray="2,2" d="M0 20 l10 0" stroke="black" stroke-width="0.5" />
    </g>
    <rect x="50" y="0" width="10" height="20" fill="url(#grid)" stroke="black" stroke-width="0.5"/>
    <rect x="1" y="1"  width="18" height="8" fill="#f999" stroke="#f99c" stroke-width="1"/>
    <rect x="1" y="11" width="18" height="8" fill="#99f9" stroke="#99fc" stroke-width="1"/>
  </g>
</svg>

Let's denote `count n` number of ways things are tiled.

* Trivially `count 0 = 1`.
* Similarly `count 1 = 1`.
* For cases when `N>=2`, there are two sub-cases, stemming from
  how we cover the top-left corner:
    * In case we cover it with a vertical domino, then we need to
      tile `n-1` so there are `count (n-1)` ways to tile like this.
    * In case we start with a horizontal domino, then we *have to* put
      one below, and we are left with `n-2`, so there are `count (n-2)`
      ways to tile like this.

Or in Haskell

```haskell
count :: Natural -> Natural
count 0 = 1
count 1 = 1
count n = count (n-1) + count (n-2)
```
</div>


<div class="speaker a">
👀
</div>

<div class="speaker b">
... ... ... Those are Fibonacci numbers! 🤯
</div>

<div class="speaker a">
Indeed 😌. Even though I like my Fibonacci numbers starting from 0. Now this
implementation is not very computationally efficient, is it? Can we do any
better?
</div>

Easy-peasy, let's just build it from bottom up, start from 0, and name the
function properly.

<div class="speaker b">
Indeed, that is exponential-ish (well, technically the time
complexity is also Fibonacci).

```haskell
fib = (!!) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

And now we are linear... well, except for complexity of multiplication because
as the values grow quite fast the complexity of multiplication cannot really
be considered constant.
</div>

The interviewer seems pleased. Almost as if your answer brought up some
pleasant memory from good-old uni times. You lost the track of time but
somehow you know you are not done yet.

<div class="speaker a">
Yes, numbers would be way too big way too fast, but we'll deal with that
later.

Let's first have a look at size of the stack. Your recursive definition of
Fibonacci numbers list would work pretty well for purposes of printing all of
them, but used like this, if I have decided to use your function while
limiting my stack with something like `+RTS -K64k` I probably would not be able
to get to `fib 2000` without a `*** Exception: stack overflow`.
</div>

<div class="speaker b">
I see what you are hinting at. Issue is not with the recursion as recursive
calls are tail calls. In this case it is the same laziness that allows us to
define an infinite list of Fibonacci numbers defers the evaluation of addition
and once we force evaluation at the end to be able to show the value, it
causes the stack to explode. We can force the evaluation with `seq`, or we can
just use
[bang patterns](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html#bang-patterns-informal)
to sprinkle in some strictness.

```haskell
fib = go 0 1
  where
    go !a !_ 0 = a
    go a b n = go b (a+b) (n-1)
```
</div>

<div class="speaker a">
Sweet. That is a pretty decent linear(ish) implementation... can we do any
better?
</div>

Now you finally feel the interview is getting to the interesting parts. From
depths of your memory you try to pull that
[Fastest Fib in the West](https://wiki.haskell.org/The_Fibonacci_sequence#Fastest_Fib_in_the_West).
It is unlikely that you'll manage to do that exact thing from the top of your
head, but getting to logarithmic complexity should not be too difficult with
the logarithmic-complexity exponentiation trick!

<div class="speaker b">
The last implementation makes it quite explicit what is happening as we
calculate the Fibonacci number from the bottom up:

In every step we remember two neighbouring numbers in the sequence
starting from (`0`, `1`) and then on each iteration we move up one step.
This can be done with matrix multiplication! If given pair of `[a b]`
we get to the next step easily.

$$
\begin{bmatrix}
a & b \\
\end{bmatrix}
\times
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}
=
\begin{bmatrix}
b & a+b \\
\end{bmatrix}
$$

And now we just start from `[0 1]` and multiply it by `n`-th power of our
update matrix.

$$
\begin{bmatrix}
0 & 1 \\
\end{bmatrix}
\times
\begin{bmatrix}
0 & 1 \\
1 & 1 \\
\end{bmatrix}^n
=
\begin{bmatrix}
\mathrm{fib}(n) & \mathrm{fib}(n+1) \\
\end{bmatrix}
$$

Which allows us to use "The Exponentiation Trick"™ and drive the
complexity to logarithmic.
</div>

<div class="speaker a">
Good to see someone who remembers that matrix multiplication
has more applications than just making [haiku](2026-01-07-no-limit-storage.html).

I would like to see an implementation that avoids
some duplicated calculations that matrix-based solution
has due to the symmetry of the matrix used, please.
</div>

...

What will happen next? Will we see some type-level
magic? Will they get to Pisano period? Will that help?
Will our candidate turn out to be a "can do that" or "can't do that"?
Only time will show... [stay tuned](2026-01-25-tech-interview-ii.html){rel="next"}.
