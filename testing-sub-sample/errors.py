from math import log, pi


def log_fac_big(n: int) -> float:
    """
    Approximation of log(n!) based on https://en.wikipedia.org/wiki/Stirling%27s_approximation.
    Suitable for big n. error term is O(1/n).
    """
    assert n > 1000
    return n * log(n) - n + 0.5*log(2*n*pi)


def log_fac_1(n: int) -> float:
    acc = 1
    for i in range(2, n + 1):
        acc *= i
    return log(acc)


def log_fac_2(n: int) -> float:
    acc = 0.0
    for i in range(1, n + 1):
        acc += log(i)
    return acc

# for i in range(10):
#     # print(i, log_fac_1(10**i) - log_fac_2(10**i))
#     if i > 0:
#         print(f"10^{i}", log_fac_1(10**i) - log_fac_big(10**i))


def comb(n: int, k: int) -> int:
    if k > n:
        return 0
    # the bigger the k, the less work we do
    k = max(k, n - k)
    acc = 1

    for i in range(k + 1, n + 1):
        acc *= i

    for i in range(2, n - k + 1):
        acc //= i

    return acc

def alpha(total: int, samples: int, errors: int) -> float:
    """
    Alpha is the probability of all `samples` taken from the population
    of size `total` being good despite there being `errors` errors in
    the population.
    """

    assert total > 0
    assert samples >= 0
    assert errors >= 0

    if samples + errors > total:
        return 0.0

    if samples < 1:
        return 1.0

    # cases when errors are in untested slots divided by all cases for `errors` errors.
    return comb(total - samples, errors) / comb(total, errors)

def confidence(total: int, samples: int, errors: int) -> float:
    """
    Confidence is `1 - Alpha`.
    """

    return 1 - alpha(total, samples, errors)


def max_errors(total: int, samples: int, target_confidence: float) -> int:
    l = 0
    r = total - samples
    while l < r:
        print(f"{l = }, {r = }")
        pivot = (l + r) // 2
        print(f"{pivot = }")
        c = confidence(total, samples, pivot)
        print(f"{c = }")
        if target_confidence < c:
            r = pivot - 1
        else:
            l = pivot + 1
    return l

def max_errors_log(total: int, samples: int, target_confidence: float) -> int:
    assert samples > 0, "Neutral face"
    target_log = log(target_confidence)
    l = 0
    r = total - samples
    while l < r:
        print(f"{l = }, {r = }")
        pivot = (l + r) // 2
        print(f"{pivot = }")
        c = confidence_log(total, samples, pivot)
        print(f"{c = }")
        if target_log < c:
            r = pivot - 1
        else:
            l = pivot + 1
    return l


def example(n: int):
    """
    >>> example(5)
    errs | 1      2      3      4      5      samples
    -----+-------------------------------------------
       1 | 0.2000 0.4000 0.6000 0.8000 1.0000
       2 | 0.4000 0.7000 0.9000 1.0000 1.0000
       3 | 0.6000 0.9000 1.0000 1.0000 1.0000
       4 | 0.8000 1.0000 1.0000 1.0000 1.0000
       5 | 1.0000 1.0000 1.0000 1.0000 1.0000

    So for 5 element set if we check 2 samples,
    without observing erros, we are 100% confident
    there are no 4 or 5 errors, 90% confident there are
    not 3 error, 70% confident there are not 2 errors
    and 40% confident there is not 1 error.

    This also means that if we are asking for confidence 1,
    we are confident with probability 1 that there are not
    4 errors, but if we relax it to 95% (anything 1 < <= 0.9),
    we are confident that there are not 3 errors.

    This is still bit weird to me.
    """

    for e in range(1,n + 1):
        for s in range(1,n + 1):
            print(f"{confidence(n,s,e):1.04f}", end=" ")
        print()


example(5)

print(f"{max_errors(100_000, 1000, 0.99) = }")
