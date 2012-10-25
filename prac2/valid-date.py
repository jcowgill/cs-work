# Tests if a given date is valid

# The number of days in each month
_MONTH_DAYS = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

def is_leap_year(year):
    '''Returns true if the given year is a Gregorian leap year'''
    return (year % 4 == 0) and ((year % 100 != 0) or (year % 400 == 0))

def is_valid_date(day, month, year):
    '''Returns true if the given date is valid in the Gregorian calendar'''

    # Throw out invalid months
    if month < 1 or month > 12:
        return False

    # Check leap days
    if month == 2 and day == 29:
        return is_leap_year(year)
    else:
        return day >= 1 and day <= _MONTH_DAYS[month - 1]

# Get inputs
date = raw_input("Enter date to test (dd/mm/yyyy): ")

# Split date and get result
date_parts = date.split('/')

if is_valid_date(int(date_parts[0]), int(date_parts[1]), int(date_parts[2])):
    print date, "is a valid date"
else:
    print date, "is NOT a valid date"
