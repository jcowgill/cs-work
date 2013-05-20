'''
Base functions and classes used by the maze challenge

18 Oct 2012
James Cowgill
'''

import turtle

# Some maze drawing constants
_DRAW_SIZE = 20         # Size of one square in the maze (in "turtle" units)
_DRAW_GREY = "gray"     # Colour for grey part of the maze
_DRAW_BLACK = "black"   # Colour for black parts of the maze
_DRAW_GREEN = "green"   # Colour for green dot
_DRAW_RED = "red"       # Colour for red dot
_DRAW_PATH = "blue"     # Colour for paths


class _Wall:
    '''Enumeration of wall representations (used for storing the maze)'''
    NONE = 0
    WEST = 1
    SOUTH = 2
    BOTH = WEST | SOUTH


class Direction:
    '''Enumeration of directions you can travel in the maze'''
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3


class Maze:
    '''
    Represents a maze and provides basic functions for analyzing it

    To create a maze, use Maze.read_...
    '''

    def __init__(self, data):
        '''
        Creates a new maze using the given data

        You probably do not want this, use a Maze.read_... instead

        data is a 2D list with the y value chosen first (ie data[y][x]).
        All the row lists must be the same length.
        The values in the row list must be constants from the _Wall class
        '''

        def fix_row(row):
            '''Copies the given row and forces the WEST edge to be blocked'''
            new_row = row[:]
            new_row[0] |= _Wall.WEST
            return new_row

        # Validate maze height
        if len(data) == 0:
            raise ValueError("maze cannot be empty")

        # Validate maze width
        width = len(data[0])

        if width == 0:
            raise ValueError("maze cannot be empty")

        for row in data:
            if len(row) != width:
                raise ValueError("all rows in the maze must be equal width")

        # Clone all data while forcing west side to be blocked
        my_data = [fix_row(row) for row in data]

        # Force south edge to be blocked
        last_row = my_data[-1]
        for i in range(0, width):
            last_row[i] |= _Wall.SOUTH

        # Store data
        self._data = my_data

        # Cache size
        self._size = (width, len(my_data))

    def __repr__(self):
        return "<Maze {0[0]} x {0[1]}>".format(self.size)

    @staticmethod
    def read_str(data):
        '''
        Reads a maze from a string of data

        The maze must have 1 line per row and can use these symbols:
        <space> = No walls
           |    = Wall on LEFT (west)
           _    = Wall on BOTTOM (south)
           L    = Wall on BOTH

        All rows must be the same length
        '''

        def parse_row(row):
            '''Converts a row read by read_str to a list of _Wall entries'''
            result = []

            for char in row:
                # Translate character
                if char == ' ':
                    wall = _Wall.NONE
                elif char == '|':
                    wall = _Wall.WEST
                elif char == '_':
                    wall = _Wall.SOUTH
                elif char == 'L':
                    wall = _Wall.BOTH
                else:
                    raise ValueError("maze row contained invalid character: " +
                                     char)

                # Store character
                result.append(wall)

            return result

        # One awesome one liner...
        return Maze([parse_row(row) for row in data.splitlines()])

    @staticmethod
    def read_file(file):
        '''Reads a maze from a file (see read_str)'''
        return Maze.read_str(file.read())

    # Maze functions

    @property
    def size(self):
        '''Returns the size of the maze as the tuple (width, height)'''

        return self._size

    def can_move(self, pos, direction):
        '''
        Returns a boolean saying weather you can move a given direction
        while standing at the given position

        pos must be a tuple (x, y)
        direction must be a constant from the Direction class
        '''

        # Get x and y from tuple
        x, y = pos

        # Validate position
        if x < 0 or y < 0 or x >= self._size[0] or y >= self._size[1]:
            raise IndexError("pos is not a valid position in the maze")

        # Each direction is handled differently
        if direction == Direction.WEST:
            # Check current position's WEST flag
            return (self._data[y][x] & _Wall.WEST) == 0

        elif direction == Direction.SOUTH:
            # Check current position's SOUTH flag
            return (self._data[y][x] & _Wall.SOUTH) == 0

        elif direction == Direction.EAST:
            # Check for edge of map OR WEST flag of right position
            return x != (self._size[0] - 1) and (self._data[y][x + 1] & _Wall.WEST) == 0

        else:
            # Check for edge of map OR SOUTH flag of upper position
            return y != 0 and (self._data[y - 1][x] & _Wall.SOUTH) == 0

    def enum_moves(self, pos):
        '''
        Enumerates the list of directions which can be made from the given
        position
        '''

        result = []

        # Get x and y from tuple
        x, y = pos

        # Validate position
        if x < 0 or y < 0 or x >= self._size[0] or y >= self._size[1]:
            raise IndexError("pos is not a valid position in the maze")

        # Check my flags
        if (self._data[y][x] & _Wall.WEST) == 0:
            result.append(Direction.WEST)

        if (self._data[y][x] & _Wall.SOUTH) == 0:
            result.append(Direction.SOUTH)

        # Check other positions's flags
        if x != (self._size[0] - 1) and (self._data[y][x + 1] & _Wall.WEST) == 0:
            result.append(Direction.EAST)

        if y != 0 and (self._data[y - 1][x] & _Wall.SOUTH) == 0:
            result.append(Direction.NORTH)

        return result

    @staticmethod
    def calc_move(pos, direction):
        '''Calculates the new position after moving the given direction'''
        x, y = pos

        if direction == Direction.NORTH:
            return (x, y - 1)
        elif direction == Direction.WEST:
            return (x - 1, y)
        elif direction == Direction.SOUTH:
            return (x, y + 1)
        else:   # EAST
            return (x + 1, y)

    def draw(self):
        '''Draws the maze using turtle (after clearing the screen)'''

        # Get maze size
        width, height = self.size

        # Reset turtle
        turtle.reset()
        turtle.hideturtle()
        turtle.pencolor(_DRAW_BLACK)

        # Move turtle to top left corner
        turtle.penup()
        turtle.goto(-(_DRAW_SIZE * width) / 2, (_DRAW_SIZE * height) / 2)
        home = turtle.pos()
        turtle.pendown()

        # Draw top and right sides
        turtle.forward(_DRAW_SIZE * width)
        turtle.right(90)
        turtle.forward(_DRAW_SIZE * height)

        turtle.penup()
        turtle.goto(home)

        # Draw each square in the maze
        for row in self._data:
            for square in row:
                # Start off at top left corner, facing down, pen up, black pen

                turtle.pendown()

                # Drawing west side?
                if (square & _Wall.WEST) == 0:
                    # No, use gray
                    turtle.pencolor(_DRAW_GREY)

                # Draw west side
                turtle.forward(_DRAW_SIZE)
                turtle.left(90)

                # Drawing south side?
                if (square & _Wall.SOUTH) == 0:
                    turtle.pencolor(_DRAW_GREY)
                else:
                    turtle.pencolor(_DRAW_BLACK)

                # Draw south side
                turtle.forward(_DRAW_SIZE)
                turtle.right(90)

                # Move to next square
                turtle.penup()
                turtle.backward(_DRAW_SIZE)
                turtle.pencolor(_DRAW_BLACK)

            # Advance to next row
            turtle.left(90)
            turtle.backward(_DRAW_SIZE * width)
            turtle.right(90)
            turtle.forward(_DRAW_SIZE)

        # Draw start and end dots
        turtle.goto(home)
        turtle.forward(_DRAW_SIZE / 2)
        turtle.left(90)
        turtle.forward(_DRAW_SIZE / 2)
        turtle.dot(_DRAW_SIZE / 2, _DRAW_GREEN)

        turtle.forward(_DRAW_SIZE * (width - 1))
        turtle.right(90)
        turtle.forward(_DRAW_SIZE * (height - 1))
        turtle.dot(_DRAW_SIZE / 2, _DRAW_RED)

    def draw_path(self, positions):
        '''
        Draws the path given by a position list
        '''

        def position_to_turtle(pos):
            '''Converts a maze position to a turtle position'''
            return (home_x + _DRAW_SIZE * pos[0], home_y - _DRAW_SIZE * pos[1])

        # Get maze size
        width, height = self.size

        # Prepare turtle
        home_x = (-(_DRAW_SIZE * width) / 2) + (_DRAW_SIZE / 2)
        home_y = ((_DRAW_SIZE * height) / 2) - (_DRAW_SIZE / 2)

        turtle.showturtle()
        turtle.pencolor(_DRAW_PATH)

        # Move to star
        turtle.penup()
        turtle.goto(home_x, home_y)
        turtle.pendown()

        # Draw the path
        for pos in positions:
            turtle.goto(position_to_turtle(pos))
