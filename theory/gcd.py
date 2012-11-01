'''
Calculates Greatest Common Divisor

31 Oct 2012
James Cowgill
'''

import time


def remainder_fast(a, b):
    '''Fast remainder calculation'''
    return a % b


def remainder_slow(a, b):
    '''Slow (repeated subtraction) remainder calculation'''

    # This doesn't bother to keep track of the quotient
    while a >= b:
        a -= b

    return a


def gcd(a, b, rem=remainder_fast):
    '''Calculates the greatest common divisor of the 2 inputs'''

    # Force inputs to integers and swap if necessary
    a = int(a)
    b = int(b)
    a, b = max(a, b), min(a, b)

    # Start gcd loop
    while True:
        r = rem(a, b)

        if r == 0:
            return b

        a = b
        b = r


if __name__ == '__main__':
    # Do tests
    start = time.perf_counter()
    print(gcd(123456789, 987654321))
    mid = time.perf_counter()
    print(gcd(123456789, 987654321, remainder_slow))
    end = time.perf_counter()

    # Print timing results
    print("Fast version took", format((mid - start) * 1000, '.2f'), "ms")
    print("Slow version took", format((end - mid) * 1000, '.2f'), "ms")
    print("This is", format((end - mid) / (mid - start), '.2f'), \
            "times slower")
