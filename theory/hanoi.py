# Towers of Hanoi
# James Cowgill
#


def toh(number, start, finish, spare, move=None):
    '''Prints the solution to the Towers of Hanoi problem

    number = number of discs
    start = starting pole (str)
    finish = finish pole (str)
    spare = spare pole (str)'''

    # Check move is ok
    if move is None:
        move = [1]

    # Handle 1 and 2 specially
    if number == 1:
        print str(move[0]) + ": Move disc 1 from", start, "to", finish
        move[0] += 1

    elif number == 2:
        print str(move[0]) + ": Move disc 1 from", start, "to", spare
        move[0] += 1
        print str(move[0]) + ": Move disc 2 from", start, "to", finish
        move[0] += 1
        print str(move[0]) + ": Move disc 1 from", spare, "to", finish
        move[0] += 1

    else:
        # Recursive step
        toh(number - 1, start, spare, finish, move)
        print str(move[0]) + ": Move disc", number, "from", start, "to", finish
        move[0] += 1
        toh(number - 1, spare, finish, start, move)

if __name__ == '__main__':
    toh(13, 'A', 'B', 'C')
