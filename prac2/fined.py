# Speed limit fine calculator

# Get inputs
limit = int(raw_input("Enter the speed limit: "))
actual_speed = int(raw_input("Enter the actual speed: "))

# Legal speed?
if actual_speed > limit:
    print "Speed is illegal"

    # Calculate fine
    fine = 100 + 5 * (actual_speed - limit)

    if actual_speed > 90:
        fine += 200

    print "Fined", fine
    
else:
    print "Speed is legal"
