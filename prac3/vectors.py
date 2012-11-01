# Vectors

def vector_scalar_product(number, vector):
    return [number * x for x in vector]

def vector_addition(v1, v2):
    return [a + b for a, b in zip(v1, v2)]

print "Scalar Product"
number = int(raw_input("Enter number: "))
vector = input("Enter vector in python style (with [] and commas) ")
print vector_scalar_product(number, vector)

print
print "Vector Addition"
v1 = input("Enter vector 1 in python style (with [] and commas) ")
v2 = input("Enter vector 2 in python style (with [] and commas) ")
print vector_addition(v1, v2)
