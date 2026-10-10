---
title: "Online Probability Estimation"
author: xkollar
tags: Math, Probability, Stub
---

Maybe one day I'll turn this into a real article,
but for now here are just some points and a picture:

The key question is:

> After `n` runs without an error,
> what is the lowest error rate `P_err`,
> that we can be C-confident we would have already observed an error.

(Lowest/highest can be kinda slippery here, but think
about extremes: for fail rate "almost 100%"
it would be very unlikely that we did not observe any error,
while with fail rate "almost 0%" not observing error would not be very suprising.)

* TL;DR, for large enough n (usually >30), good approximation for 95% confidence is `3/n`.
  (E.g. after 100 runs I am 95% confident the error rate is `≤3%`.)
* For cases where we don't have "total population"
  and samples just come to us one by one we can't use
  the approach from [the previous article](2025-12-14-when-you-cant-test-everything.html).
* But we can use the part from "Stumbling in the Dark"
  and realize that after `n` samples with no errors
  `P(observed no errors) = (1-P_err)^n`
* Some math is possible (<https://en.wikipedia.org/wiki/Rule_of_three_(statistics)>),
  but also just binary search to find `P_err` for
  a given confidence.
* Logs are again useful.

To build up an intuition, here is example with 5 samples

```txt
 Errs | Samples   | Probability   | Comment
------+-----------+---------------+------------------
    0 | . . . . . | (1-P_err)^5   | <- You are here!
------+-----------+---------------+-----------------
    1 | . . . . X | 1-(1-P_err)^5 | Part with errors
      | . . . X . |               | that we haven't
      | . . X . . |               | observed.
      | . X . . . |               |
      | X . . . . |               | For 95% confidence
------+-----------+               | we need this part
    2 | . . . X X |               | to be at least 95%
      | . . X . X |               |
      | . . X X . |               |
      | . X . . X |               |
      | . X . X . |               |
      | . X X . . |               |
      | X . . . X |               |
      | X . . X . |               |
      | X . X . . |               |
      | X X . . . |               |
------+-----------+               |
    3 | . . X X X |               |
      | . X . X X |               |
      | . X X . X |               |
      | . X X X . |               |
      | X . . X X |               |
      | X . X . X |               |
      | X . X X . |               |
      | X X . . X |               |
      | X X . X . |               |
      | X X X . . |               |
------+-----------+               |
    4 | . X X X X |               |
      | X . X X X |               |
      | X X . X X |               |
      | X X X . X |               |
      | X X X X . |               |
------+-----------+               |
    5 | X X X X X |               |
```

To have c-confidence, we want

```
(1-P_err)^n <= (1-c)
```

But also high values of `P_err`
are not very interesting (For example
`P_err = 1` satisfies the inequality
but is completely uninteresting),
so we want to find as small a `P_err`
as possible, which is getting us to

```
(1-P_err)^n = (1-c)
```

And again, running things through logs makes it a smidge faster.

```
log (1-P_err)^n = log (1-c)
```

```python-render
# copy: testing-sub-sample/online.py
import matplotlib.pyplot as plt

from online import find_err

samples = range(1,1000)

plt.title('Confidence-based Error Probabilities (and Heuristics)')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Samples')
plt.ylabel('Estimated upper bound of P_err')

data = [
    (0.5,  0.7, "blue"),
    (0.75, 1.4, "orange"),
    (0.95, 3.0, "green"),
    (0.99, 4.61, "red"),
]

for confidence, rule, color in data:
    plt.plot(samples, [find_err(x, confidence) for x in samples], color=f"{color}", label = f"{confidence = }")
    plt.plot(samples, [rule/x for x in samples], label=f"{rule}/n", linestyle="dotted", color=f"dark{color}")

plt.legend(loc='best')

plt.savefig("/dev/stdout", format="svg")
```

(Upper bound of confidence band `(0, P_err)`.
