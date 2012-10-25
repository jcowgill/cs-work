# Tests if a year is a leap year

def is_leap_year(year):
    '''Returns true if the given year is a Gregorian leap year'''
    return (year % 4 == 0) and ((year % 100 != 0) or (year % 400 == 0))

# Get inputs
year = int(raw_input("Enter year to test: "))

if is_leap_year(year):
    print year, "is a leap year"
else:
    print year, "is NOT a leap year"
