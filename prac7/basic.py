# Recusion Practical
# James Cowgill
#

import math


def is_power(a, b):
    '''Returns True if a is a power of b'''
    if a == 1:
        return True
    elif a <= 0 or b == 1:
        return False
    else:
        return (a % b == 0) and is_power(a / b, b)


def fibonacci_rec(n):
    '''Returns the nth fibonnaci number'''
    if n <= 1:
        return 1
    else:
        return fibonacci_rec(n - 1) + fibonacci_rec(n - 2)


def fibonacci_iter(n):
    '''Returns the nth fibonacci number'''
    a = 1
    b = 1

    # Add together until we've done it n times
    for _ in range(1, n):
        a, b = a + b, a

    return a

SQRT_5 = math.sqrt(5)
GOLDEN_RATIO = (1 + SQRT_5) / 2


def fibonacci_best(n):
    '''Returns the nth fibonacci number'''
    return int(0.5 + (GOLDEN_RATIO ** (n + 1)) / SQRT_5)
