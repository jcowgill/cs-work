# Tree drawing using L-Systems
#  James Cowgill
#

from lsystem import LSystem
from turtle import RawTurtle, Screen


class TurtleDrawState(object):
    '''The current turtle drawing state'''

    def __init__(self, turtle, fd_length, angle):
        '''Initializes a new turtle drawing state'''
        self.turtle = turtle
        self.fd_length = fd_length
        self.angle = angle
        self.__stack = []

    @staticmethod
    def default_render_rules():
        '''Returns a list containing the default rendering rules'''
        return {
                    "F": TurtleDrawState.forwards,
                    "f": TurtleDrawState.forwards_no_draw,
                    "+": TurtleDrawState.rotate_left,
                    "-": TurtleDrawState.rotate_right,
                    "|": TurtleDrawState.rotate_180,
                    "[": TurtleDrawState.push,
                    "]": TurtleDrawState.pop,
                    "(": TurtleDrawState.inc_angle,
                    ")": TurtleDrawState.dec_angle,
                    "<": TurtleDrawState.inc_fd_length,
                    ">": TurtleDrawState.dec_fd_length,
                }

    def forwards(self):
        '''Moves the turtle forwards by the current fdLength'''
        self.turtle.fd(self.fd_length)

    def forwards_no_draw(self):
        '''
        Moves the turtle forwards by the current fdLength
        without drawing a line
        '''

        self.turtle.penup()
        self.turtle.fd(self.fd_length)
        self.turtle.pendown()

    def rotate_left(self):
        '''Rotates the turtle left by the current angle'''
        self.turtle.lt(self.angle)

    def rotate_right(self):
        '''Rotates the turtle right by the current angle'''
        self.turtle.rt(self.angle)

    def rotate_180(self):
        '''Rotates the turtle 180 degrees'''
        self.turtle.rt(180)

    def inc_fd_length(self):
        '''Increment fd_length by 10%'''
        self.fd_length *= 1.1

    def dec_fd_length(self):
        '''Decrement fd_length by 10%'''
        self.fd_length *= 0.9

    def inc_angle(self):
        '''Increment angle by 10%'''
        self.angle *= 1.1

    def dec_angle(self):
        '''Decrement angle by 10%'''
        self.angle *= 0.9

    def push(self):
        '''Pushes the current position and angle onto the stack'''
        self.__stack.append((self.turtle.pos(), self.turtle.heading()))

    def pop(self):
        '''Pops the current position and angle off the stack'''
        top = self.__stack.pop()

        self.turtle.penup()
        self.turtle.goto(top[0])
        self.turtle.pendown()
        self.turtle.seth(top[1])


class TurtleLSystem(LSystem):
    '''A L-System which renders its output using a python turtle'''

    def __init__(self, start, rules):
        '''Initializes a new L-System'''
        LSystem.__init__(self, start, rules)

        # Initialize standard rendering rules + parameters
        self.fd_length = 5
        self.angle = 90
        self.__render_rules = TurtleDrawState.default_render_rules()

    @property
    def render_rules(self):
        '''Gets the list of rendering rules'''
        return self.__render_rules

    def render_turtle(self, turtle):
        '''Renders the L-System using the given turtle'''

        # Create state
        state = TurtleDrawState(turtle, self.fd_length, self.angle)

        # Render
        LSystem.render(self, self.render_rules, state)

    def render(self, screen=None):
        '''Renders the L-System on a turtle screen'''

        # Get default screen
        if screen is None:
            screen = Screen()

        # Create turtle and pass on
        self.render_turtle(RawTurtle(screen))


# Some example L-Systems
def sierpinski():
    '''L-System to create a sierpinski triangle'''
    system = TurtleLSystem("A", {"A": "B-A-B", "B": "A+B+A"})
    system.render_rules["A"] = TurtleDrawState.forwards
    system.render_rules["B"] = TurtleDrawState.forwards
    system.angle = 60

    return system


def dragon_curve():
    '''L-System to create a dragon curve'''
    system = TurtleLSystem("FX", {"X": "X+YF", "Y": "FX-Y"})
    system.angle = 90
    return system


def tall_tree():
    '''L-System to create a tall tree'''
    system = TurtleLSystem(
        "1",
        {
            "1": "FF[-2][3][+3]",
            "2": "FF+F-F-F[FFF3][+3]-F-F3",
            "3": "FF-F+F+F[2][-2]+F+F2"
        })
    system.angle = 20
    return system


def render_system(system, n=0):
    '''Renders the given system using a new turtle'''
    # Expand if requested
    system.expand(n)

    # Draw it
    turtle = RawTurtle(Screen())
    turtle.speed(0)
    turtle.ht()
    turtle.lt(90)

    system.render_turtle(turtle)


if __name__ == '__main__':
    # Test the tall tree L-System
    render_system(tall_tree(), 6)
