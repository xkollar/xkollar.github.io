from itertools import islice, permutations, combinations
from random import choice, choices

pizza_prices = choices(range(1,10), k=10)
pizza_prices = (7, 5, 5, 3, 2, 2, 3, 7, 3, 1)


def chunks(l:list[int], n:int):
    it = iter(l)
    return tuple(iter(lambda: tuple(islice(it, n)),()))


def evaluator(groups_iterable, verbose=True):
    max_savings = 0
    best_split = None
    good_stuff = set()
    count = 0

    for groups in groups_iterable:
        count += 1
        savings = sum(min(group) for group in groups if len(group) == 3)
        if savings > max_savings:
            max_savings = savings
            best_split = groups
            good_stuff = set()
        elif savings == max_savings:
            groups = tuple(sorted((tuple(sorted(s, reverse=True)) for s in groups), reverse=True))
            good_stuff.add(groups)

    if verbose:
        print("="*80)
        print(f"{count = }, {max_savings = }\n{best_split = }")
        for s in good_stuff:
            print(" ",s)
    return max_savings, best_split


def gen_v1(prices):

    for l in permutations(prices):
        yield chunks(l, 3)

def combs(src, n:int):
    src = sorted(src, reverse=True)

    def go(s):
        if len(s) < n:
            yield [tuple(s)]
        else:
            a, *t = s
            for c in set(combinations(t, n-1)):
                head = (a, *c)
                rest = list(t)
                for x in c:
                    rest.remove(x)

                for tail in go(rest):
                    yield [head, *tail]

    return go(src)

def gen_v2(prices):
    return combs(prices, 3)

def gen_v3(prices):
    yield chunks(sorted(prices, reverse=True), 3)

# evaluator(gen_v1(pizza_prices))
evaluator(gen_v2(pizza_prices))

# pizza_prices = choices(range(1,10), k=14)
# evaluator(gen_v2(pizza_prices))


for _ in range(10000):
    k = choice(range(2,12))
    pizza_prices = choices(range(1,30), k=k)
    res2, _ = evaluator(gen_v2(pizza_prices), verbose=False)
    res3, _ = evaluator(gen_v3(pizza_prices), verbose=False)
    assert res2 == res3, pizza_prices
