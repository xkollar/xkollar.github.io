from decimal import Decimal
from fractions import Fraction
from itertools import batched
from random import choices
from typing import Generic, Iterable, Literal, Protocol, Sized, TypeVar

A = TypeVar("A")
# Is there a better way to make mypy happy?
#    T = TypeVar("N", bound=numbers.Number, covariant=True) -- complex numbes don't have comparison
#    T = TypeVar("N", bound=numbers.Real, covariant=True) -- int won't work
T = TypeVar("T", int, float, Decimal, Fraction, covariant=True)


# For some reason this has to have T and not some other covariant TypeVar
class IterableSized(Generic[T], Iterable[T], Sized, Protocol): ...


def savings(groups: Iterable[IterableSized[T]]) -> T | Literal[0]:
    return sum(min(g) for g in groups if len(g) == 3)


def max_savings(prices: Iterable[T]) -> Iterable[tuple[T, ...]]:
    return batched(sorted(prices, reverse=True), 3)


if __name__ == "__main__":
    prices = choices(range(1, 20), k=7)
    solution = list(max_savings(prices))
    print(f"{solution = }")
    print(f"{savings(solution) = }")
