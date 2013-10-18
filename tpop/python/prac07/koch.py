# Koch Curve
#  James Cowgill
#

import turtle


def koch_curve(turtle, length, degree):
    '''Draws the koch curve with the given turtle

    length = size of segment
    degree = number of iterations to perform'''

    if degree == 0:
        turtle.fd(length)
    else:
        # Do one iteration
        length /= 3
        degree -= 1

        koch_curve(turtle, length, degree)
        turtle.left(60)
        koch_curve(turtle, length, degree)
        turtle.right(120)
        koch_curve(turtle, length, degree)
        turtle.left(60)
        koch_curve(turtle, length, degree)


def koch_wipe(degree):
    '''Display a koch curve but wiping the screen before hand'''
    turtle.clearscreen()
    me = turtle.Turtle()
    me.speed(0)
    me.penup()
    me.bk(400)
    me.pendown()
    koch_curve(me, 800, degree)
    me.ht()
