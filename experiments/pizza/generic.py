from decimal import Decimal
from fractions import Fraction
from itertools import batched
from random import choices
from typing import Iterable, Literal, TypeVar

T = TypeVar("T", int, float, Decimal, Fraction, covariant=True)


def savings(groups: Iterable[Iterable[T]], m: int, n: int) -> T | Literal[0]:
    "saving for given grouping for offer m+n (buy m, get n free)"
    assert m > 0
    assert n > 0
    return sum(p for g in groups for p in sorted(g)[:-m][:n])


def optimal_grouping(prices: Iterable[T], m: int, n: int) -> Iterable[tuple[T, ...]]:
    assert m > 0
    assert n > 0
    return batched(sorted(prices, reverse=True), m + n)


if __name__ == "__main__":
    m = 2
    n = 2
    prices = choices(range(1, 20), k=7)
    solution = list(optimal_grouping(prices, m, n))

    print(f"{m = }, {n = }")
    print(f"{solution = }")
    print(f"{savings(solution,m,n) = }")
