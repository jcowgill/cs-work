# ASCII code printer

sum = 0

for c in raw_input("Enter string to test: "):
    ascii_code = ord(c)
    sum += ascii_code
    print ascii_code

print "Sum =", sum
