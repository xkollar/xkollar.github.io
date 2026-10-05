from decimal import Decimal
from fractions import Fraction
import itertools
import collections
from collections import Counter, deque
import math
from typing import Iterable, Literal, TypeVar, Callable
import functools
from queue import Queue

T = TypeVar("T", int, float, Decimal, Fraction, covariant=True)

from generic import savings, optimal_grouping

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
    return savings(optimal_grouping(prices, m, n), m, n)


# prices: list[tuple[T,int]] = []
# 
# def shapely(prices: list[T], val: Callable[[list[T]], T]):
#     contributions = collections.Counter()
#     for p in itertools.permutations(enumerate(prices)):
#         value = 0
#         s = []
#         for i, x in p:
#             s.append(x)
#             new_value = val(s)
#             contributions.update({i: new_value-value})
#             value = new_value
# 
#     n = math.factorial(len(prices))
#     return [x/n for _,x in sorted(contributions.items())]
# 
# def shapely_pizza(prices, m=2, n=1):
#     return shapely(prices, lambda s: optimal_savings(tuple(sorted(s)), m, n))

class MultNnum:
    def __init__(self, n:int = 1, /):
        self.signum = 1
        self._nums = {}
        if n == 0:
            self.signum = 0
            return
        if n < 0:
            self.signum = -1
            n = -n
        i = 2
        c = 0
        while n >= i:
            d, m = divmod(n,i)
            if m == 0:
                c += 1
                n = d
            else:
                if c > 0:
                    self._nums[i] = c
                c = 0
                i+=1
        self._nums[i] = c

class Stuff:
    def __init__(self, counts):
        self._counts = counts

    @functools.cached_property
    def subset_counts(self):
        return ...

multiset = tuple[tuple[T,int], ...]

def multi_subsets(ms: multiset) -> Iterable[multiset]:
    curr = {(tuple(),ms)}
    while curr:
        nxt = {}
        for b, rem in curr:
            yield tuple(b)
            for v,c in rem:
                
        curr = nxt


for x in multi_subsets(((10,3),(12,2))):
    print(x)

# def fun(prices: typ):
# 
#     current = [tuple()]
#     nxt = []
#     print(f"{prices = }")
# 
# 
# fun(((10,3),(12,2)))



