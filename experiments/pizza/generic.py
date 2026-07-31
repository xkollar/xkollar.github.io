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


def max_savings(prices: Iterable[T], m: int, n: int) -> Iterable[tuple[T, ...]]:
    assert m > 0
    assert n > 0
    return batched(sorted(prices, reverse=True), m + n)


if __name__ == "__main__":
    prices = choices(range(1, 20), k=7)
    solution = list(max_savings(prices, 2, 2))

    print(f"{solution = }")
    print(f"{savings(solution,2,2) = }")
