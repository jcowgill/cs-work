'''
Calculates the product of distinct prime factors of a number

18 Oct 2012
James Cowgill
'''

import math

def _prime_olympiad_test_list(number, primes):
    '''Tests just using the primes in the primes'''
    
    # Calculate cutoff
    cutoff = int(math.sqrt(number))
    
    # Find primes
    for prime in primes:
        # Exit if above the cutoff
        if prime > cutoff:
            break
        
        if number % prime == 0:
            # number is NOT prime
            return False
        
    return True

def prime_olympiad_calc(number):
    '''Calculates the product of distinct prime factors of a number'''
    
    # Force number to integer and get cutoff point
    number = int(number)
    
    # Ignore numbers less than 2
    if number < 2:
        raise ValueError("numbers must be greater than 1")
    
    # Create initial list and set 2 as starting number
    primes = []
    current = 2
    result = 1
    
    # Loop through all the numbers testing for primality
    while number > 1:
        if _prime_olympiad_test_list(current, primes):
            # Add to list
            primes.append(current)
            
            # Does number divide this prime?
            if number % current == 0:
                # Add to result and remove any other of these factors
                result *= current
                
                while True:
                    number //= current
                    
                    if number % current != 0:
                        break
        
        # Next number
        current += 1
    
    # Return final result
    return result

# If this is the main module, do test now
if __name__ == '__main__':
    print(prime_olympiad_calc(input()))
