from math import sqrt

s5 = sqrt(5)

def fib(n):
    return round(((-2/(1-s5))**n - (-2/(1+s5))**n)/s5)
