---
title: "On-line probability"
author: xkollar
tags: Math, Probability, Stub
---

Maybe one day I'll this into a real article, for now just
some points and a picture:

* For cases where we don't have "total potulation"
  and samples just come to us one-by-one we can't use
  the approach from previous article.
* But we can use the part from "Stumbling in the Dark"
  and realize that after `n` samples with no erros
  `P(observed no errors) = (1-P_err)^n`
* Some math is possibe (<https://en.wikipedia.org/wiki/Rule_of_three_(statistics)>),
  but also just binary search to find `P_err` for
  given confidence.
* Logs are again useful.

```python-render
# copy: testing-sub-sample/online.py
import matplotlib.pyplot as plt

from online import find_err

samples = range(1,1000)

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

```txt
 Errs | Samples   | Pobability  | Comment
------+-----------+-------------+------------------
    0 | . . . . . | (1-P_err)^5 | <- You are here!
------+-----------+-------------+-----------------
    1 | . . . . X |             | This does not
      | . . . X . |             | matter for us
      | . . X . . |             | for purposes of
      | . X . . . |             | this article
      | X . . . . |             |
------+-----------+-------------+-----------------
    2 | . . . X X |             | ditto
      | . . X . X |             |
      | . . X X . |             |
      | . X . . X |             |
      | . X . X . |             |
      | . X X . . |             |
      | X . . . X |             |
      | X . . X . |             |
      | X . X . . |             |
      | X X . . . |             |
------+-----------+-------------+-----------------
    3 | . . X X X |             |
      | . X . X X |             |
      | . X X . X |             |
      | . X X X . |             |
      | X . . X X |             |
      | X . X . X |             |
      | X . X X . |             |
      | X X . . X |             |
      | X X . X . |             |
      | X X X . . |             |
------+-----------+-------------+-----------------
    4 | . X X X X |             |
      | X . X X X |             |
      | X X . X X |             |
      | X X X . X |             |
      | X X X X . |             |
------+-----------+-------------+-----------------
    5 | X X X X X |             |
```
