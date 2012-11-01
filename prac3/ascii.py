# ASCII code printer

total = 0

for c in raw_input("Enter string to test: "):
    ascii_code = ord(c)
    total += ascii_code
    print ascii_code

print "total =", total
