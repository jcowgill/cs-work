# Matricies
#  All matricies are lists of rows

def matrix_scalar_product(number, matrix):
    # Generate new matrix from old rows
    new = []

    for row in matrix:
        new.append([number * col for col in row])

    return new

def matrix_addition(m1, m2):
    # Generate new matrix from old rows
    new = []

    for row1, row2 in zip(m1, m2):
        new.append([a + b for a, b in zip(row1, row2)])

    return new

def matrix_transpose(matrix):
    return zip(*matrix)

def matrix_multiply(m1, m2):
    # Matrix multiplication
    new_rows = len(m1)
    new_cols = len(m2[0])
    mid_number = len(m2)
    new = []

    # Ensure mid number is the same for both matricies
    if mid_number != len(m1[0]):
        raise ValueError("matricies cannot be multiplied")

    # Process matrix
    for row_num in range(0, new_rows):
        # Create new list for this row
        row = []

        # Process each column in the row
        for col_num in range(0, new_cols):
            # Multiply m1 row by m2 col at this position
            sum = 0
            for mid_num in range(0, mid_number):
                sum += m1[row_num][mid_num] * m2[mid_num][col_num]

            row.append(sum)
            
        new.append(row)

    return new
