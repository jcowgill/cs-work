# Palendrome tester

PALENDROME_NOT = 0
PALENDROME_PARTIAL = 1
PALENDROME_FULL = 2

def palendrome_test(string):
    '''Tests whether the given string is a palendrome

    Returns one of:
    - PALENDROME_NOT     = Not a palendrome
    - PALENDROME_PARTIAL = A palendrome when ignoring punctuation
    - PALENDROME_FULL    = Al full palendrome

    This function ignores case
    '''
    
    # Ignoring case
    string = string.lower()
    
    # Setup initial positions
    palendrome_type = PALENDROME_FULL
    left = 0
    right = len(string) - 1

    # Check characters from each end
    while left <= right:
        if string[left] == string[right]:
            # Advance both positions
            left += 1
            right -= 1
            
        elif not string[left].isalnum():
            # Ignore non alpha character
            palendrome_type = PALENDROME_PARTIAL
            left += 1
        
        elif not string[right].isalnum():
            # Ignore non alpha character
            palendrome_type = PALENDROME_PARTIAL
            right -= 1

        else:
            # Completely different
            palendrome_type = PALENDROME_NOT
            break

    # Return type of palendrome
    return palendrome_type

# Do the test
result = palendrome_test(raw_input("Enter string: "))

if result == PALENDROME_NOT:
    print "Not a palendrome"
elif result == PALENDROME_FULL:
    print "Is a palendrome"
else:
    print "Is a palendrome when ignoring punctuation"
