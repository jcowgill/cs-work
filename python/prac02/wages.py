# Payment at time-and-a-half
#  Any hours worked above 40 are payed at 1.5 times the hourly rate

# Get inputs
hours = int(raw_input("Enter number of hours worked: "))
rate = int(raw_input("Enter the hourly rate: "))

# Handle normal hours first
if hours > 40:
    total = 40 * rate

    # Handle 1.5 rate
    total += (hours - 40) * rate * 1.5
else:
    total = hours * rate

# Print result
print "Wages: {0:.2f}".format(total)
