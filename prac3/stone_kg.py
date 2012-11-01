# Stones and Pounds table
print "st,lb to kg conversion table"

# Print header row
print "|st\lb |",
for lb in range(0, 14):
    print format(lb, "5d") + "|",
print

# Print each row
for st in range(0, 21):
    print (7 * 15 + 1) * '-'
    print "|" + format(st, "6d") + "|",
    
    for lb in range(0, 14):
        kg = 0.45 * (14 * st + lb)
        print format(kg, "5.1f") + "|",

    print
