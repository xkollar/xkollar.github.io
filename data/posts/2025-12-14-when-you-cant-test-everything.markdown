---
title: "When you can't test everything"
author: xkollar
tags: Math, Probability
---

In the real world there are many cases where we would like
all members of a set/population/... to satisfy certain property
but it is not practical (or possible) to test them all.

The set might be too big, individual tests themselves too time consuming, or
too expensive in some other way. Or perhaps testing is destructive and after
testing all the elements you would have nothing left.

As an example it might be a huge database with lots of entries.
Or an airport and deep searches.

In any case, you take your budget (whether that is money, time, API calls,
...) and max it out running your tests. If you found an error, you have your
answer: the system is not error-free.

But what if no errors are found? Was it a pure luck and the rest of the system
is full of errors? We can't be certain, but hopefully we can have at least
some kind of probabilistic confidence.

Or perhaps you'd like to be able to talk to the rest of the business allowing
them to understand the relationship between the resource allocation on testing
and likelihood of there being undetected errors in the system.

## The First Attempt (Stumbling in the Dark)

To make things more simple, let's say we have 5 elements in total and are able
to test only 2 of them. It might be tempting to attempt the following
analysis:

```txt
 Untested | Tested   | Cumulative Probability
----------+----------+-----------------
    X X X | . .      | P(e=3|c=2) = 1/8 (just this line)
    X X . | . .      |
    X . X | . .      |
    . X X | . .      | P(e>=2|c=2) = 1/2 (this and all previous lines)
    X . . | . .      |
    . X . | . .      |
    . . X | . .      | P(e>=1|c=2) = 7/8 (this and all previous lines)
    . . . | . .      |
```

However, unless the probability of an individual element being faulty is 0.5,
this is not the case! Imagine probability of an individual fault is 0.1. The
following table captures the probabilities.

```txt
 Untested | Probability | Cumulative
----------+-------------+------------
    X X X |       0.001 | P(e=3)  = 0.001
    X X . |       0.009 |
    X . X |       0.009 |
    . X X |       0.009 | P(e>=2) = 0.028
    X . . |       0.081 |
    . X . |       0.081 |
    . . X |       0.081 | P(e>=1) = 0.271
    . . . |       0.729 | Total   = 1
```

So without a prior knowledge of probability of an individual failure, we
can't say much.

## The Better Way

Luckily, there is something we can do! We can ask:

> If there were `n` errors, what is the probability that we missed all of them?

For the purposes of our analysis we can fix which elements are checked without
loss of generality. 👋👋

```txt
 Errs | Untested | Tested | Miss | P(seen=0|e=Errs) | Confidence Threshold
------+----------+--------+------+------------------+------------
    0 |    . . . | . .    |      | 1                | 0
------+----------+--------+------+------------------+------------
    1 |    . . . | . X    |      | 3/5              | 2/5
      |    . . . | X .    |      | = 6/10           | = 4/10
      |    . . X | . .    | !    |                  |
      |    . X . | . .    | !    |                  |
      |    X . . | . .    | !    |                  |
------+----------+--------+------+------------------+------------
    2 |    . . . | X X    |      | 3/10             | 7/10
      |    . . X | . X    |      |                  |
      |    . . X | X .    |      |                  |
      |    . X . | . X    |      |                  |
      |    . X . | X .    |      |                  |
      |    . X X | . .    | !    |                  |
      |    X . . | . X    |      |                  |
      |    X . . | X .    |      |                  |
      |    X . X | . .    | !    |                  |
      |    X X . | . .    | !    |                  |
------+----------+--------+------+------------------+------------
    3 |    . . X | X X    |      | 1/10             | 9/10
      |    . X . | X X    |      |                  |
      |    . X X | . X    |      |                  |
      |    . X X | X .    |      |                  |
      |    X . . | X X    |      |                  |
      |    X . X | . X    |      |                  |
      |    X . X | X .    |      |                  |
      |    X X . | . X    |      |                  |
      |    X X . | X .    |      |                  |
      |    X X X | . .    | !    |                  |
------+----------+--------+------+------------------+------------
    4 |    . X X | X X    |      | 0                | 1
      |    X . X | X X    |      |                  |
      |    X X . | X X    |      |                  |
      |    X X X | . X    |      |                  |
      |    X X X | X .    |      |                  |
------+----------+--------+------+------------------+------------
    5 |    X X X | X X    |      | 0                | 1
```

Notice that this is okay to do, as all rows in a given group have the same
probability regardless of what is the probability of an individual element
being faulty! **Assumption: Errors are randomly distributed across all elements
with equal probability.** Also notice that we know for sure that there are not
4 nor 5 errors (as we would have observed a faulty element).

Now we can start asking questions:

> What is the smallest number of errors we are 100% sure we would have
> found at least one?

Well... 4. While this one is not very exciting, we can go further:

> What is the smallest number of errors we are at least 90% confident we would
> have found at least one?

Looking at our analysis: 3. And how would you answer:

> What is the smallest number of errors we are at least 95% confident we would
> have fount at least one?

Again: 4. Thinking about these cases will allow us to notice patterns with
possibility to generalize. (And hopefully now the `P(seen=0|e=0) = 1` makes
sense too.)

## General Case 🫡💼

In general, probability for not having seen any error after taking `k` samples
from set of total size `n` given there are `e` errors in the set is number of
ways how place `e` errors in untested (`n-k`) positions divided by the number
of ways to place `e` errors in all positions, or in <abbr title="mathematics (inside joke)">mafs</abbr>:

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mi>P</mi>
  <mo>=</mo>
  <mfrac>
    <mrow data-mjx-texclass="ORD">
      <mrow data-mjx-texclass="OPEN">
        <mo minsize="1.2em" maxsize="1.2em">(</mo>
      </mrow>
      <mfrac linethickness="0">
        <mrow>
          <mi>n</mi>
          <mo>&#x2212;</mo>
          <mi>k</mi>
        </mrow>
        <mi>e</mi>
      </mfrac>
      <mrow data-mjx-texclass="CLOSE">
        <mo minsize="1.2em" maxsize="1.2em">)</mo>
      </mrow>
    </mrow>
    <mrow data-mjx-texclass="ORD">
      <mrow data-mjx-texclass="OPEN">
        <mo minsize="1.2em" maxsize="1.2em">(</mo>
      </mrow>
      <mfrac linethickness="0">
        <mi>n</mi>
        <mi>e</mi>
      </mfrac>
      <mrow data-mjx-texclass="CLOSE">
        <mo minsize="1.2em" maxsize="1.2em">)</mo>
      </mrow>
    </mrow>
  </mfrac>
</math>

And here are some graphs in case it helps you understand
things. Symmetry is not coincidental.

```python-render
# copy: testing-sub-sample/confidence.py
import matplotlib.pyplot as plt

from confidence import alpha

n = 1000

fig, axs = plt.subplots(1,2)
fig.suptitle(f"Detection Confidence: {n=}")

ax1, ax2 = axs

for e in [1,10,100]:
    samples = range(n+1)
    prob_miss = [alpha(n, k, e) for k in samples]
    ax1.plot(samples, prob_miss, label=f"{e} actual errors")

ax1.set(xlabel="Samples (k)")

for k in [1,10,100]:
    errors = range(0, n+1)
    prob_miss = [alpha(n, k, e) for e in errors]
    ax2.plot(errors, prob_miss, label=f"{k} samples")

ax2.set(xlabel="Actual Errors (e)")

for ax in axs.flat:
    ax.legend(loc='best')
    ax.set(ylabel="P(Miss All Errors)")
    ax.label_outer()

plt.savefig("/dev/stdout", format="svg")
```

## Complexity

Pure mathematician would perhaps be happy here and consider the problem
solved. (Apologies to ones that would not.) However, the premise of this
article is that the size of the set is large, and calculating large factorials
brings its own challenges. Consider the following monstrosity before
simplification!
([Check Wikipedia for the complexity of factorial](https://en.wikipedia.org/wiki/Computational_complexity_of_mathematical_operations)
which is better than naive multiplication of numbers from `1` to `n`.
Python's `math.factorial`{.python} uses [Divide-and-conquer factorial algorithm](https://github.com/python/cpython/blob/main/Modules/mathintegermodule.c)
based on <http://www.luschny.de/math/factorial/binarysplitfact.html>.)

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mi>P</mi>
  <mo>=</mo>
  <mfrac>
    <mrow data-mjx-texclass="ORD">
      <mrow data-mjx-texclass="OPEN">
        <mo minsize="1.2em" maxsize="1.2em">(</mo>
      </mrow>
      <mfrac linethickness="0">
        <mrow>
          <mi>n</mi>
          <mo>&#x2212;</mo>
          <mi>k</mi>
        </mrow>
        <mi>e</mi>
      </mfrac>
      <mrow data-mjx-texclass="CLOSE">
        <mo minsize="1.2em" maxsize="1.2em">)</mo>
      </mrow>
    </mrow>
    <mrow data-mjx-texclass="ORD">
      <mrow data-mjx-texclass="OPEN">
        <mo minsize="1.2em" maxsize="1.2em">(</mo>
      </mrow>
      <mfrac linethickness="0">
        <mi>n</mi>
        <mi>e</mi>
      </mfrac>
      <mrow data-mjx-texclass="CLOSE">
        <mo minsize="1.2em" maxsize="1.2em">)</mo>
      </mrow>
    </mrow>
  </mfrac>
  <mo>=</mo>
  <mfrac>
    <mfrac>
      <mrow>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </mrow>
      <mrow>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
        <menclose notation="updiagonalstrike">
          <mi>e</mi>
          <mo>!</mo>
        </menclose>
      </mrow>
    </mfrac>
    <mfrac>
      <mrow>
        <mi>n</mi>
        <mo>!</mo>
      </mrow>
      <mrow>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
        <menclose notation="updiagonalstrike">
          <mi>e</mi>
          <mo>!</mo>
        </menclose>
      </mrow>
    </mfrac>
  </mfrac>
  <mo>=</mo>
  <mfrac>
    <mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo stretchy="false">)</mo>
      <mo>!</mo>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo stretchy="false">)</mo>
      <mo>!</mo>
    </mrow>
    <mrow>
      <mi>n</mi>
      <mo>!</mo>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo stretchy="false">)</mo>
      <mo>!</mo>
    </mrow>
  </mfrac>
  <mo>=</mo>
  <mfrac>
    <mrow>
      <menclose notation="updiagonalstrike">
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </menclose>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo stretchy="false">)</mo>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x2026;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo>+</mo>
      <mn>1</mn>
      <mo stretchy="false">)</mo>
      <menclose notation="updiagonalstrike">
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </menclose>
    </mrow>
    <mrow>
      <mi>n</mi>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x2026;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo>+</mo>
      <mn>1</mn>
      <mo stretchy="false">)</mo>
      <menclose notation="updiagonalstrike">
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </menclose>
      <menclose notation="updiagonalstrike">
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </menclose>
    </mrow>
  </mfrac>
  <mo>=</mo>
  <mfrac>
    <mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo stretchy="false">)</mo>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x2026;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>e</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo>+</mo>
      <mn>1</mn>
      <mo stretchy="false">)</mo>
    </mrow>
    <mrow>
      <mi>n</mi>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x2026;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo>&#x22C5;</mo>
      <mrow data-mjx-texclass="ORD"></mrow>
      <mo stretchy="false">(</mo>
      <mi>n</mi>
      <mo>&#x2212;</mo>
      <mi>k</mi>
      <mo>+</mo>
      <mn>1</mn>
      <mo stretchy="false">)</mo>
    </mrow>
  </mfrac>
</math>

The simplification improved things: the number of multiplications
now depends only on `k`. This sounds great at first but still is not ideal:
for `1_000_000` total elements and sample size `10_000`
intermediate results can be on the scale of `~10^60000`. Consider the
following Python session 😅.

```txt
>>> a = 1
>>> for x in range(10_000):
...     a *= (1_000_000-x)
... 
>>> a
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ValueError: Exceeds the limit (4300 digits) for integer string conversion; use sys.set_int_max_str_digits() to increase the limit
>>> float(a)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
OverflowError: int too large to convert to float
>>> import sys
>>> sys.set_int_max_str_digits(60000)
>>> float(str(a))
inf
```

Not very useful. And at this scale we also need to take into account even the
complexity of the multiplication itself! (Because arbitrary precision
numbers.) Maybe we could do some smart pairing to keep that at minimum, but
let's try something more fun!

## A Log-ical Sidestep

In the wild we won't care about absolute precision (and later on we'll be just
comparing things during searches for various things), so there is a fun side
step we can do: move to a logarithmic space! (For the rest of the article
we'll use log to mean logarithm with base e.)

Logarithms are cool because:

* They make big numbers more manageable.
* They turn multiplication/division into addition-subtraction.
* Preserve ordering: <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mi mathvariant="normal">&#x2200;</mi>
  <mi>x</mi>
  <mo>,</mo>
  <mi>y</mi>
  <mo>&#x2208;</mo>
  <msup>
    <mrow data-mjx-texclass="ORD">
      <mi mathvariant="double-struck">R</mi>
    </mrow>
    <mo>+</mo>
  </msup>
  <mo>:</mo>
  <mi>x</mi>
  <mo>&#x2264;</mo>
  <mi>y</mi>
  <mo stretchy="false">&#x27FA;</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>x</mi>
  </mrow>
  <mo>&#x2264;</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>y</mi>
  </mrow>
</math>.

One might be tempted to add `log`s all they way through
our previous equations and that way alleviate some
pains we were experiencing. However, we'll do one
more funky side-step.

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>P</mi>
  </mrow>
  <mo>=</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mfrac>
      <mrow>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </mrow>
      <mrow>
        <mi>n</mi>
        <mo>!</mo>
        <mo stretchy="false">(</mo>
        <mi>n</mi>
        <mo>&#x2212;</mo>
        <mi>e</mi>
        <mo>&#x2212;</mo>
        <mi>k</mi>
        <mo stretchy="false">)</mo>
        <mo>!</mo>
      </mrow>
    </mfrac>
  </mrow>
  <mo>=</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mo stretchy="false">(</mo>
    <mi>n</mi>
    <mo>&#x2212;</mo>
    <mi>k</mi>
    <mo stretchy="false">)</mo>
    <mo>!</mo>
  </mrow>
  <mo>+</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mo stretchy="false">(</mo>
    <mi>n</mi>
    <mo>&#x2212;</mo>
    <mi>e</mi>
    <mo stretchy="false">)</mo>
    <mo>!</mo>
  </mrow>
  <mo>&#x2212;</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>n</mi>
    <mo>!</mo>
  </mrow>
  <mo>&#x2212;</mo>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mo stretchy="false">(</mo>
    <mi>n</mi>
    <mo>&#x2212;</mo>
    <mi>e</mi>
    <mo>&#x2212;</mo>
    <mi>k</mi>
    <mo stretchy="false">)</mo>
    <mo>!</mo>
  </mrow>
</math>

At this point you might be thinking:

> What is going on? Maybe there is a faster way to
calculate `log(n!)` ... 🤔

And you'd be right: [Stirling's approximation](https://en.wikipedia.org/wiki/Stirling%27s_approximation).

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>n</mi>
    <mo>!</mo>
  </mrow>
  <mo>=</mo>
  <mi>n</mi>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mi>n</mi>
  </mrow>
  <mo>&#x2212;</mo>
  <mi>n</mi>
  <mo>+</mo>
  <mstyle displaystyle="false" scriptlevel="0">
    <mfrac>
      <mn>1</mn>
      <mn>2</mn>
    </mfrac>
  </mstyle>
  <mi>log</mi>
  <mo data-mjx-texclass="NONE">&#x2061;</mo>
  <mrow data-mjx-texclass="ORD">
    <mn>2</mn>
    <mi>&#x3C0;</mi>
    <mrow data-mjx-texclass="ORD"></mrow>
    <mi>n</mi>
  </mrow>
  <mo>+</mo>
  <mi>O</mi>
  <mo stretchy="false">(</mo>
  <mstyle displaystyle="false" scriptlevel="0">
    <mfrac>
      <mn>1</mn>
      <mi>n</mi>
    </mfrac>
  </mstyle>
  <mo stretchy="false">)</mo>
</math>

Which is amazing as error goes down with `n` going up! Let's run
a quick test!

```txt
>>> from math import factorial, log, pi
>>> for exp in range(6):
...     n = 10**exp
...     print(exp, log(factorial(n)) - (n * log(n) - n + 0.5*log(2*pi*n)))
... 
0 0.08106146679532733
1 0.008330563433359472
2 0.000833330555565226
3 8.333333062182646e-05
4 8.333328878507018e-06
5 8.330680429935455e-07
```

Looks good, let's write some python!

```python
from math import exp, factorial, log, pi

PRECISION = 3


def log_fac(n: int) -> float:
    if n >= 10**PRECISION:
        assert n >= 0
        return n * log(n) - n + 0.5 * log(2 * n * pi)
    else:
        return log(factorial(n))


def log_alpha(total: int, samples: int, errors: int) -> float:
    assert total > 0
    assert samples <= total, "Samples more than total?"
    assert errors <= total, "Erros more than total?"
    if errors + samples > total:
        return float("-inf")
    return (
        log_fac(total - samples)
        + log_fac(total - errors)
        - log_fac(total)
        - log_fac(total - errors - samples)
    )


def alpha(total: int, samples: int, errors: int) -> float:
    """
    Probability of no errors being observed in `samples` samples
    out of `total` elements, given there are `errors` errors.
    """
    return exp(log_alpha(total, samples, errors))
```

Compare with and without precision correction.

```txt
>>> PRECISION = 0
>>> alpha(5, 2, 3)
0.09489739502331558
>>> alpha(50, 20, 3)
0.20718475090635413
>>> alpha(500, 200, 3)
0.21513426941998892
>>> alpha(5000, 2000, 3)
0.21591358271716066

>>> PRECISION = 4
>>> alpha(5, 2, 3)
0.10000000000000002
>>> alpha(50, 20, 3)
0.207142857142857
>>> alpha(500, 200, 3)
0.21513388222227164
>>> alpha(5000, 2000, 3)
0.21591357887611928
```

## Exploring

Now that we have build some basic building blocks,
let's try to find answers to some frivolous questions!

> Given dataset of size n, what is the smallest number of errors we are
> c-confident we would have discovered an error by k samples?

```python
def min_errors(total: int, samples: int, confidence_threshold: float) -> int:
    """
    Finds minimal number of erros we are at least `confidence_threshold`
    confident we would discover an error by checking `samples` samples
    out of `total` elements.
    """
    left = 0
    right = total - samples + 1
    a = 1 - confidence_threshold
    log_a = log(a)
    while left < right:
        pivot = (left + right) // 2
        if log_alpha(total, samples, pivot) > log_a:
            left = pivot + 1
        else:
            right = pivot
    return left
```

> Given dataset of size n, what is the smallest number of samples we need to
> take to be at least c-confident we would have discovered an error if
> there were x errors present?

```python
def min_samples(total: int, errors: int, confidence_threshold: float) -> int:
    """
    Finds minimal number of samples to be at least `confidence_threshold`
    confident we would have discovered an error if there were
    `errors` errors in `total` elements.
    """
    left = 0
    right = total - errors + 1
    a = 1 - confidence_threshold
    log_a = log(a)
    while left < right:
        pivot = (left + right) // 2
        if log_alpha(total, pivot, errors) > log_a:
            left = pivot + 1
        else:
            right = pivot
    return left
```

## Note on Precission

Thanks to small rounding errors some threshold values won't work exactly,
but we are more likely to run this code for large numbers where
that is kinda okay.

```txt
>>> alpha(5,2,3)
0.10000000000000002
```

## Severity of a Bug

One more interesting thing I'd like to leave you with is a though on what is a
bug and how do I feel about "not knowing whether there is an error lurking
just around the corner".

Under "normal" circumstances, "system has a problem" is a binary thing; either
yes or no. But here, unless an issue was discovered within chosen samples, we
don't know.

Let's say we have set of size `1_000_000`, and we have found out that after we
have tested `1_000` members without finding an issue, we are 95% confident
that if there were `min_errors(1_000_000, 1_000, 0.95) = 2_990` errors, we
would have found at least 1.

One way to think about it is, that we are 95% confident
that if there was a systemic error (a bug) that would
impact ~3% of members, we would have noticed it.

And while this is not the only possible way to measure
impact of the bug, it is certainly an interesting one.

## Conclusion

We got to the point where, even without knowing much about probabilities of
the underlying issue, we were able to gain some curious insights!

**Takeaways**:

* Combinatorial analysis gives confidence bounds *without* knowing individual failure rates.
* Stirling's approximation enables computation at scale.
* Test design: Use `min_samples` to justify resource allocation.
* Reporting: Use `min_errors` to state "We're 95% confident undetected errors < X",
* What fraction of elements (users/products/...) are impacted by a bug
  can be an interesting measure of severity.

## Afterword

Consider this my journal on a journey trying to figure out some fun things!
There might be errors/imprecisions/typos, ... I might have even committed
couple of horrible things here. If you have noticed something and care enough:
please let me know!

