---
title: "Testing Subsets"
author: xkollar
tags: Math, Probability
---

In the real world there are many cases where we would like
all members of a set/population/... to satisfy a certain property
but it not practical (or possible) to test them all.

Set might be too big, individual test might take too long,
or it might be too expensive.

As an example it might be a huge database with lots of entries.
Or an airport and deep searches.

In any case, you take your budget (whether that is money,
time, API calls, ...) and max it out running your tests.
If you found your error, you have your answer: the system is compromised.

But what if you haven't found any errors? You'd probably like
to be able to express what have you learned in terms of probabilities
that there are errors.

## The First Attempt (Stumbling in the Dark)

To make things more simple, let's say we have 5 elements in total and are
able to test only 2. It might be tempting to attempt the following analysis:

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

However, unless the probability of an individual element being faulty is 0.5, this is not the case!
Imagine probability of an individual fault is 0.1. The following table captures the probabilities.

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

For the purposes of our analysis we can fix which elements are checked without loss of generality. 👋👋

```txt
 Errs | Untesed | Tested | Miss | P(seen=0|e=Errs) | Confidence Thresohld
------+---------+--------+------+------------------+------------
    0 |   . . . | . .    |      | 1                | 0
------+---------+--------+------+------------------+------------
    1 |   . . . | . X    |      | 3/5              | 2/5
      |   . . . | X .    |      | = 6/10           | = 4/10
      |   . . X | . .    | !    |                  |
      |   . X . | . .    | !    |                  |
      |   X . . | . .    | !    |                  |
------+---------+--------+------+------------------+------------
    2 |   . . . | X X    |      | 3/10             | 7/10
      |   . . X | . X    |      |                  |
      |   . . X | X .    |      |                  |
      |   . X . | . X    |      |                  |
      |   . X . | X .    |      |                  |
      |   . X X | . .    | !    |                  |
      |   X . . | . X    |      |                  |
      |   X . . | X .    |      |                  |
      |   X . X | . .    | !    |                  |
      |   X X . | . .    | !    |                  |
------+---------+--------+------+------------------+------------
    3 |   . . X | X X    |      | 1/10             | 9/10
      |   . X . | X X    |      |                  |
      |   . X X | . X    |      |                  |
      |   . X X | X .    |      |                  |
      |   X . . | X X    |      |                  |
      |   X . X | . X    |      |                  |
      |   X . X | X .    |      |                  |
      |   X X . | . X    |      |                  |
      |   X X . | X .    |      |                  |
      |   X X X | . .    | !    |                  |
------+---------+--------+------+------------------+------------
    4 |   . X X | X X    |      | 0                | 1
      |   X . X | X X    |      |                  |
      |   X X . | X X    |      |                  |
      |   X X X | . X    |      |                  |
      |   X X X | X .    |      |                  |
------+---------+--------+------+------------------+------------
    5 |   X X X | X X    |      | 0                | 1
```

Notice that this is okay to do, as all rows in a given group have the same probability regardless
of what is probability of an individual element being faulty! (Assuming that probability is same on every position.)
Also notice that we know for sure that there are not 4 nor 5 errors (as we would have observed a faulty element).

Now we can start asking questions:

> What is the smallest number of errors we are 100% sure we would have discovered?

Well... 4. While this one is not very exiting, we can go further:

> What is the smallest number of errors we are at least 90% confident we would have discovered?

Looking at our analysis: 3. And how would you answer:

> What is the smallest number of errors we are at least 95% confident we would have discovered?

Again, 4. Thinking about these cases will allow us to notice patterns with possibility to generalize.
(And hopefully now the `P(seen=0|e=0) = 0` makes sense too.)

## General Case 🫡💼
