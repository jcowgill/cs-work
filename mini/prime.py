'''
Tests weather some numbers are prime wile caching results in a file

17 Oct 2012
James Cowgill
'''

import math

# Open primes file and read the primes from it
primes_file = open("primes", "a+")
primes_file.seek(0)
prime_list = [int(x) for x in primes_file.readlines()]

def _prime_test_with_list(number):
    '''Tests just using the primes in the primes_list'''
    
    # Calculate cutoff
    cutoff = int(math.sqrt(number))
    
    # Find primes
    for prime in prime_list:
        # Exit if above the cutoff
        if prime > cutoff:
            break
        
        if number % prime == 0:
            # number is NOT prime
            return False
        
    return True

def prime_test(number):
    '''Tests whether the given integer is a prime number'''
    
    # Force number to integer and get cutoff point
    number = int(number)
    
    # Ignore numbers less than 2
    if number < 2:
        return False
    
    # Find starting prime
    if len(prime_list) == 0:
        # Reset list with 2
        first_prime = 2
        prime_list.append(2)
        primes_file.write("2\n")
    else:
        # Start with the last prime in the file
        first_prime = prime_list[len(prime_list) - 1]
        
    # Calculate all the primes we need
    for i in range(first_prime + 1, int(math.sqrt(number))):
        if _prime_test_with_list(i):
            # Add to list of primes
            prime_list.append(i)
            primes_file.write(str(i) + "\n")
            
    # Finally, test the prime using just the primes list
    return _prime_test_with_list(number)

# If this is the main module, do test now
if __name__ == '__main__':
    # Get number to test
    print("Primality Tester - James Cowgill")
    print(" Enter a blank line to exit")
    
    # Find some primes!
    while(True):
        print()
        in_str = input("Enter number to test: ")
        
        # Empty?
        if len(in_str) == 0:
            break
        
        # Test and print result
        if prime_test(in_str):
            print(str(int(in_str)) + " is prime")
        else:
            print(str(int(in_str)) + " is NOT prime")
    