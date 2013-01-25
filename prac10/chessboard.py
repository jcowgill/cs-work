# Chessboard printer
#  James Cowgill
#

def print_chessboard(n):
    '''Prints an n x n chessboard'''

    # Calculate contents of all lines
    top_line = '+' + ('-' * n) + '+'
    middle_part = '# ' * ((n + 1) // 2)
    even_line = '|' + middle_part[:n] + '|'
    odd_line = '| ' + middle_part[:n - 1] + '|'

    # Print them
    print top_line
    for i in range(n):
        if i % 2 == 0:
            print even_line
        else:
            print odd_line

    print top_line


if __name__ == '__main__':
    print_chessboard(input("Enter the dimension of the chessboard: "))
