# Fractal Trees
#  James Cowgill
#

from __future__ import division
import turtle

# Size of branches
BRANCH_SIZE = 10

# Angle BETWEEN sub branches in degrees
BRANCH_ANGLE = 5


def draw_tree(pen, depth):
    '''Draws a tree using the given pen and for the given tree depth'''

    # Exit if we've reached the bottom
    if depth == 0:
        return

    # Calculate this branch's sizes
    branch_len = depth * BRANCH_SIZE
    branch_ang = depth * BRANCH_ANGLE

    # Draw my branch (size proportion to depth)
    pen.fd(branch_len)

    # Draw sub-branches
    pen.lt(branch_ang / 2)
    draw_tree(pen, depth - 1)
    pen.rt(branch_ang)
    draw_tree(pen, depth - 1)
    pen.lt(branch_ang / 2)

    # Return to starting position
    pen.bk(branch_len)


if __name__ == '__main__':
    # Initialize turtle
    pen = turtle.Turtle()

    pen.penup()
    pen.speed(0)
    pen.pensize(2)
    pen.lt(90)
    pen.bk(300)
    pen.pendown()

    # Draw simple tree
    draw_tree(pen, 7)
