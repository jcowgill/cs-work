'''
A-Star Maze Solver

This doesn't do what the turtle mini-challenge says I should do but I just felt
like doing some A-Star :)

20 May 2013
James Cowgill
'''

import turtle
import Queue
from maze_base import *

def solve_maze(maze):
    '''
    Solves the given maze using the A-Star pathfinding algorithm

    The shortest path = path taking least number of steps
    (ie the shortest manhattan distance).

    Returns a iteratable object containing positions the turtle moves to on each step.
    The list contains the end position but not the starting position.
    If there is no path from the start to the end, returns None.
    '''

    def distance(a, b):
        '''Returns the manhattan distance between the given points'''
        return abs(a[0] - b[0]) + abs(a[1] - b[1])

    # Get start and end positions
    start = (0, 0)
    end = (maze.size[0] - 1, maze.size[1] - 1)

    # Setup data structures
    open_set = Queue.PriorityQueue()    # Set of positions to look at
    closed_set = set()      # Positions looked at and will not change anymore
    best = {}               # Best known score + parent tuple

    # Add start position
    open_set.put((distance(end, start), start))
    best[start] = (0, None)

    # Start loop
    while not open_set.empty():
        current = open_set.get()[1]

        # Reached the end?
        if current == end:
            # Generate list of posiitions in the best path
            position_list = []

            while current != start:
                position_list.append(current)
                current = best[current][1]

            return reversed(position_list)

        # Move current to the closed set
        closed_set.add(current)

        # Try and add the surrounding positions
        for direction in maze.enum_moves(current):
            new_pos = Maze.calc_move(current, direction)

            # Ignore if in the closed set
            if new_pos in closed_set:
                continue;

            # Calculate new score
            new_score = best[current][0] + 1

            # Update score if there hasn't been one before or if this is better
            if (new_pos not in best) or (new_score < best[new_pos][0]):
                f_score = new_score + distance(end, new_pos)
                if new_pos not in best:
                    open_set.put((f_score, new_pos))

                best[new_pos] = (new_score, current)

    # Failed to find a path
    return None
