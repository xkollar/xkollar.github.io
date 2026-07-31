from decimal import Decimal
from fractions import Fraction
import itertools
import collections
import math
from typing import Iterable, Literal, TypeVar, Callable
import functools

T = TypeVar("T", int, float, Decimal, Fraction, covariant=True)

from generic import savings, max_savings

COUNTERS = collections.Counter()

def call_count(fun):
    key = f"{fun.__module__}.{fun.__name__}"

    @functools.wraps(fun)
    def counted(*args, **kwargs):
        COUNTERS[key] += 1
        return fun(*args, **kwargs)

    return counted

@functools.lru_cache()
@call_count
def optimal_savings(prices: list[T], m: int, n: int) -> T:
    return savings(max_savings(prices, m, n), m, n)


def shapely(prices: list[T], val: Callable[[list[T]], T]):
    contributions = collections.Counter()
    for p in itertools.permutations(enumerate(prices)):
        value = 0
        s = []
        for i, x in p:
            s.append(x)
            new_value = val(s)
            contributions.update({i: new_value-value})
            value = new_value

    n = math.factorial(len(prices))
    return [x/n for _,x in sorted(contributions.items())]

def shapely_pizza(prices, m=2, n=1):
    return shapely(prices, lambda s: optimal_savings(tuple(sorted(s)), m, n))
