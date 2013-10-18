# Parser for Deterministic Context-Free L-Systems
#  James Cowgill
#


class LSystem(object):
    '''A deterministic context-free L-System'''

    def __init__(self, start, rules):
        '''
        Initializes a new L-System

        start = (iterable of var_type) containing the initial state of the
                L-System (on iteration 0)
        rules = dictionary of (var_type -> list of var_type) which contains
                how to expand one character into another list
        '''

        self.__rules = rules
        self.__state = start
        self.__iteration = 0

    @property
    def iteration(self):
        '''Gets the current iteration number'''
        return self.__iteration

    def __iter__(self):
        '''Gets an iterator which iterates over the current L-System state'''
        return iter(self.__state)

    def __len__(self):
        '''Gets the number of objects in the current state'''
        return len(self.__state)

    def expand_once(self):
        '''Performs one L-System expansion iteration'''

        new_state = []

        # Iterate over the current state and create a new state
        for c in self.__state:
            # Expand rule for this character
            if c in self.__rules:
                new_state.extend(self.__rules[c])
            else:
                new_state.append(c)

        # Update object state
        self.__state = new_state
        self.__iteration += 1

    def expand(self, n=1):
        '''Performs the given number of L-System expansion iterations'''

        for _ in range(n):
            self.expand_once()

    def render(self, render_rules, state):
        '''
        Renders the L-System (eg on a screen)

        render_rules = dictionary of var_type -> function(state) describing how
                       to render each character
        state        = opaque argument to pass to the render rules
        '''

        for c in self:
            if c in render_rules:
                render_rules[c](state)
