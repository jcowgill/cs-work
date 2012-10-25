'''
Solves sudoku puzzles

20 Oct 2012
James Cowgill
'''


def _list_clone(data):
    '''Performs a deep copy of a list of list'''
    new_list = []
    for i in range(0, len(data)):
        new_list.append(data[i][:])
        
    return new_list


def _solve_recursive(data, x, y):
    '''Recursive solve starting at position x, y'''

    solution_list = []
    
    if len(data) == 9:
        square_size = 3
    else:
        square_size = 2

    # Find next empty space
    while data[y][x] != ' ':
        # Advance grid pointer
        x += 1

        if x == len(data):
            # Next row
            y += 1
            x = 0

            if y == len(data):
                # Got to the end without failing
                return [_list_clone(data)]

    # Try each of the possible values from 1 to 9
    for value_num in range(1, len(data) + 1):
        value = str(value_num)

        # Check current row
        if value in data[y]:
            continue

        # Check current column
        fail = False
        for row in range(0, len(data)):
            if data[row][x] == value:
                fail = True
                break

        if fail:
            continue

        # Check current square
        square_y = y - (y % square_size)
        square_x = x - (x % square_size)

        for row in range(square_y, square_y + square_size):
            for column in range(square_x, square_x + square_size):
                if data[row][column] == value:
                    fail = True

        if fail:
            continue

        # OK, this position is valid so try it recursively
        data[y][x] = value
        solution_list.extend(_solve_recursive(data, x, y))
        data[y][x] = ' '

    # Return the list of solutions found
    return solution_list


def solve(data):
    '''
    Solves the given sudoku puzzle

    data must be a list of lists of characters (must be '1' to '9' or ' ')
    Returns a list of solutions (this will be empty if the puzzle is impossible)
    '''

    # Must be 4x4 or 9x9
    if len(data) != 4 and len(data) != 9:
        raise ValueError("data must be a 4x4 or 9x9 list of lists")

    # Validate widths
    for row in data:
        if len(row) != len(data):
            raise ValueError("all rows in the grid must be correct length")

    # Recursively solve
    return _solve_recursive(_list_clone(data), 0, 0)


if __name__ == '__main__':
    # Get the puzzle
    size = int(input("Enter puzzle size (4 or 9): "))
    if size != 4 and size != 9:
        print("Size must be 4 or 9")
    else:
        # Get puzzle
        data = []
        for i in range(0, size):
            data.append(list(input()))

        print()

        # Solve it and print solutions
        solutions = solve(data)
        
        if len(solutions) > 0:
            print("Found " + str(len(solutions)) + " solutions")
            
            for entry in enumerate(solutions):
                print()
                print("Solution " + str(entry[0] + 1))
                print("-------------")
                for row in entry[1]:
                    print(''.join(row))
        else:
            print("Puzzle could not be solved")
