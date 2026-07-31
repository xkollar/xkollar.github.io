from basic import savings, max_savings


def optimal_savings(prices):
    return savings(max_savings(prices))


def pizza_proportional(prices):
    total = sum(prices)
    d = optimal_savings(prices)
    return [p * (total - d) / total for p in prices]
