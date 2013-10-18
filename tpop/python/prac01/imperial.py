# Imperial Weight to Metric
#

# Get inputs
stones = int(raw_input("Enter number of stones: "))
pounds = int(raw_input("Enter number of pounds: "))

# Calculate + print result
result = (stones * 14 + pounds) * 0.453592
print stones, "st", pounds, "lb =", result, "kg"
