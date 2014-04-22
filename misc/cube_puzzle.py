'''
Cube Puzzle solver
 James Cowgill
'''

import operator

DIR_SURROUND = {'U': "LRFB",
                'D': "LRFB",
                'L': "UDFB",
                'R': "UDFB",
                'F': "UDLR",
                'B': "UDLR"}

DIR_TO_TUPLE = {'R': (1,  0,  0),
                'L': (-1, 0,  0),
                'U': (0,  1,  0),
                'D': (0, -1,  0),
                'B': (0,  0,  1),
                'F': (0,  0, -1)}


class Node():
    '''An IMMUTABLE linked list entry'''
    def __init__(self, value, next=None):
        self.value = value
        self.next = next

    def __repr__(self):
        return repr(self.to_list())

    def to_list(self):
        if self.next is None:
            return [self.value]
        else:
            return [self.value] + self.next.to_list()

    @classmethod
    def from_list(cls, lst):
        '''Convert python list to linked list of Nodes'''
        current = None

        for val in reversed(lst):
            current = Node(val, current)

        return current


class PuzzleState():
    '''
    Object describing the state of a puzzle

     config    = linked list containing length of next puzzle pieces
     size      = size of puzzle (always a cube)
     blocks    = linked list of blocks which are allocated
                 first item in list = last block placed
     last_dir  = last direction (UDLRFB)
    '''
    def __init__(self, size, config, blocks=None, last_dir='U'):
        if blocks is None:
            middle = size - 1
            blocks = Node((middle, middle, middle))

        self.size = size
        self.config = config
        self.blocks = blocks
        self.last_dir = last_dir

    def clone(self):
        '''Creates an identical copy of this object'''
        return PuzzleState(self.size, self.config, self.blocks, self.last_dir)

    def clone_use_config(self):
        '''Like clone but use 1 config entry up'''
        return PuzzleState(self.size, self.config.next, self.blocks, self.last_dir)

    def is_within_bounds(self):
        '''Returns true if the blocks are exceeding the bounds of the puzzle'''
        min_vals = [self.size] * 3
        max_vals = [-1] * 3

        current = self.blocks
        while current is not None:
            # Update min + max vals
            for d in range(3):
                min_vals[d] = min(min_vals[d], current.value[d])
                max_vals[d] = max(max_vals[d], current.value[d])

            current = current.next

        # Exceeding if differences exceed size
        for d in range(3):
            if max_vals[d] - min_vals[d] >= self.size:
                return False

        return True

    def place_block(self, pos, new_dir):
        '''Places a block at the given position'''
        # Search blocks to ensure block is not duplicated
        current = self.blocks
        while current is not None:
            if current.value == pos:
                return False

            current = current.next

        self.blocks = Node(pos, self.blocks)
        self.last_dir = new_dir
        return True


def tuple_add(a, b):
    return tuple(map(operator.add, a, b))


def solve(state):
    '''
    Solve puzzle starting with given state

    Returns None if puzzle cannot be solved
    Returns string of moves if it can
    '''

    def place_blocks():
        for _ in range(my_config):
            last_block = new_state.blocks.value
            if not new_state.place_block(tuple_add(last_block, d_tuple), d):
                return False

        return True

    # Get config or die if finished (sucessfully)
    if state.config is None:
        print(state.blocks)
        return ""

    my_config = state.config.value

    # Enumerate possible blocks + moves
    dirs = DIR_SURROUND[state.last_dir]
    for d in dirs:
        new_state = state.clone_use_config()
        d_tuple = DIR_TO_TUPLE[d]

        # Place blocks + validate state
        if place_blocks() and new_state.is_within_bounds():
            # Recurse
            result = solve(new_state)
            if result is not None:
                return d + result

    # No solution found
    return None


if __name__ == '__main__':
    loc_config = Node.from_list([2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2])
    print(solve(PuzzleState(3, loc_config)))
