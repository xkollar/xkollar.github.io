from math import log

PRECISION = 0.00001

def prec_round(x: float) -> float:
    return PRECISION * round(x / PRECISION)


def find_err(total: int, confidence: float) -> float:
    left = 0.0
    right = 1.0

    target = log(1 - confidence)

    while right - left > PRECISION:
        pivot = (left + right) / 2
        if total * log(1 - pivot) < target:
            right = pivot
        else:
            left = pivot

    return prec_round((left + right) / 2)
