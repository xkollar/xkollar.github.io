from math import exp, factorial, log, pi

PRECISION = 3


def log_fac(n: int) -> float:
    if n >= 10**PRECISION:
        assert n >= 0
        return n * log(n) - n + 0.5 * log(2 * n * pi)
    else:
        return log(factorial(n))


def log_alpha(total: int, samples: int, errors: int) -> float:
    assert total > 0
    assert samples <= total, "Samples more than total?"
    assert errors <= total, "Erros more than total?"
    if errors + samples > total:
        return float("-inf")
    return (
        log_fac(total - samples)
        + log_fac(total - errors)
        - log_fac(total)
        - log_fac(total - errors - samples)
    )


def alpha(total: int, samples: int, errors: int) -> float:
    """
    Probability of no errors being observed in `samples` samples
    out of `total` elements, given there are `errors` errors.
    """
    return exp(log_alpha(total, samples, errors))


def min_errors(total: int, samples: int, confidence_threshold: float) -> int:
    """
    Finds minimal number of erros we are at least `confidence_threshold`
    confident we would discover an error by checking `samples` samples
    out of `total` elements.
    """
    left = 0
    right = total - samples + 1
    a = 1 - confidence_threshold
    log_a = log(a)
    while left < right:
        pivot = (left + right) // 2
        if log_alpha(total, samples, pivot) > log_a:
            left = pivot + 1
        else:
            right = pivot
    return left


def min_samples(total: int, errors: int, confidence_threshold: float) -> int:
    """
    Finds minimal number of samples to be at least `confidence_threshold`
    confident we would have discovered an error if there were
    `errors` errors in `total` elements.
    """
    left = 0
    right = total - errors + 1
    a = 1 - confidence_threshold
    log_a = log(a)
    while left < right:
        pivot = (left + right) // 2
        if log_alpha(total, pivot, errors) > log_a:
            left = pivot + 1
        else:
            right = pivot
    return left
