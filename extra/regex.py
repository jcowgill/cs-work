# Toy Regex Compiler
# James Cowgill
#

# This is just a toy for experimenting with proper regex compilers
# if you actually want to do some regex - use the python re module
#

import collections


class _RefHashable():
    '''Allows mutable classes to be used as keys (keyed by reference)'''
    def __hash__(self):
        '''Allows states to be placed in sets'''
        return id(self)

    def __eq__(self, other):
        '''Reference comparison'''
        return self is other


class _DfaState(_RefHashable, dict):
    '''Represents a DFA state (characters -> other states)'''

    def __init__(self, accepting=False):
        '''Creates a new blank DFA state'''
        self.accepting = accepting


class _NfaState(_RefHashable, collections.defaultdict):
    '''Represents an NFA state (character sets -> other states)

    The None character set represents elipson transitions'''

    def __init__(self):
        '''Creates a new blank NFA state'''
        super(_NfaState, self).__init__(list)
        self.accepting = False


_Fragment = collections.namedtuple("_Fragment", ["start", "end"])


class Regex:
    '''Each instance of this class represents a compiled regex pattern'''

    def __init__(self, pattern):
        '''Creates a new regex pattern object and compiles it'''
        self.start = Regex.__dfa_simplify(
                        Regex.__subset_construction(
                        Regex.__create_nfa(pattern)))

    @staticmethod
    def __create_nfa(pattern):
        '''Parses pattern and returns a complete NFA

        This function returns the initial _NfaState object'''

        def process_operator(op):
            '''Generates states for the given operator'''

            if op == 'c':
                # Concatenation
                right = frag_stack.pop()
                left = frag_stack.pop()

                # Add transition from left.end to right.start
                left.end[None].append(right.start)

                # Push new fragment
                frag_stack.append(_Fragment(left.start, right.end))

            elif op == '|':
                # Union
                right = frag_stack.pop()
                left = frag_stack.pop()

                # Create splitter and joiner states
                splitter = _NfaState()
                joiner = _NfaState()

                # Add the transitions
                splitter[None] = [left.start, right.start]
                left.end[None].append(joiner)
                right.end[None].append(joiner)

                # Push new fragment
                frag_stack.append(_Fragment(splitter, joiner))

            elif op == '+':
                # At least one
                frag = frag_stack.pop()

                # Create a new state at the end
                new_end = _NfaState()

                # Add transitions to the end state
                frag.end[None].append(frag.start)
                frag.end[None].append(new_end)

                # Push new fragment
                frag_stack.append(_Fragment(frag.start, new_end))

            elif op == '?':
                # Optional
                frag = frag_stack.pop()

                # Create surrounding states
                splitter = _NfaState()
                joiner = _NfaState()

                # Add the transitions
                splitter[None] = [frag.start, joiner]
                frag.end[None].append(joiner)

                # Push new fragment
                frag_stack.append(_Fragment(splitter, joiner))

            elif op == '*':
                # At least zero
                #  Same as +?
                process_operator('+')
                process_operator('?')

            elif op == '(':
                raise ValueError("Error in regex pattern")

            else:
                assert False, "Invalid operator given"

        def op_precedence(op):
            '''Returns the precedence of the given operator'''

            if op == '|':
                return 1
            elif op == 'c':
                return 2
            elif op == '(':
                return 0
            else:
                return 3

        def push_operator(op):
            '''Pushes the given operator after processing any other
            operators'''

            while operator_stack and \
                op_precedence(op) <= op_precedence(operator_stack[-1]):

                # Process this operator
                process_operator(operator_stack.pop())

            # Push our operator
            operator_stack.append(op)

        def concat_push():
            '''Pushes concat (c) if is_concat is true'''
            if is_concat:
                push_operator('c')

        frag_stack = []         # Stack of NFA fragments
        operator_stack = []     # Stack of regex operators ('c' = concat)
        is_concat = False       # True if next input could result in a concat

        # Begin processing the pattern
        for c in pattern:
            # Find special character c
            if c == '(':
                # Check for concatenations
                if is_concat:
                    push_operator('c')

                # Push onto stack immediately
                operator_stack.append('(')
                is_concat = False

            elif c == ')':
                # Process operators until we find a (
                while operator_stack[-1] != '(':
                    process_operator(operator_stack.pop())

                # Discard (
                operator_stack.pop()
                is_concat = True

            elif c == '|' or c == '?' or c == '+' or c == '*':
                # Normal operator - push onto stack
                push_operator(c)

                # Concatenation is enabled except for unions
                is_concat = (c != '|')

            else:
                # Normal character
                if is_concat:
                    push_operator('c')

                # Create new transition for this character
                left = _NfaState()
                right = _NfaState()
                left[c].append(right)

                # Push fragment
                frag_stack.append(_Fragment(left, right))
                is_concat = True

        # Process any operators on the stack
        while operator_stack:
            process_operator(operator_stack.pop())

        # Ensure there is exactly 1 fragment on the stack
        if len(frag_stack) != 1:
            raise ValueError("Error in regex pattern")

        # Mark end as the accepting state and return starting state
        frag_stack[0].end.accepting = True
        return frag_stack[0].start

    @staticmethod
    def __subset_construction(nfa):
        '''Takes the initial state of an NFA and converts it to a DFA'''

        def elipson_closure(states):
            '''Calculates the elipson closure of the set of NFA states'''

            # Copy list to stack and result
            stack = list(states)
            result = set(states)

            # Process each state
            while stack:
                state = stack.pop()

                # Go through each elipson transition
                if None in state:
                    for sub_state in state[None]:
                        if not sub_state in result:
                            # Add to result and states to process
                            result.add(sub_state)
                            stack.append(sub_state)

            return frozenset(result)

        def move(states, char):
            '''Returns set of states which you can reach after a char input'''

            result = set()

            # Go through each input set and union the transition list
            for state in states:
                result.update(state[char])

            return result

        def any_accepting(states):
            '''Returns true if any of the states are accepting'''
            return any((state.accepting for state in states))

        # Create initial state
        start_closure = elipson_closure([nfa])
        start_state = _DfaState(any_accepting(start_closure))
        nfa_to_dfa = {start_closure: start_state}

        # Initialise states to process
        to_process = [(start_closure, start_state)]

        # Start converting
        while to_process:
            current, current_state = to_process.pop()

            # Process each possible character transition from the states
            char_transitions = set()
            for state in current:
                char_transitions.update(state.iterkeys())

            # Process these characters
            for char in char_transitions:
                # Ignore elipson
                if char is None:
                    continue

                # Generate new closure after the move
                new_closure = elipson_closure(move(current, char))

                # Lookup closure in database
                if new_closure in nfa_to_dfa:
                    # Extract existing state
                    new_state = nfa_to_dfa[new_closure]

                else:
                    # Generate a new state which will need processing
                    new_state = _DfaState(any_accepting(new_closure))
                    nfa_to_dfa[new_closure] = new_state
                    to_process.append((new_closure, new_state))

                # Add transition
                current_state[char] = new_state

        # Done, return starting state
        return start_state

    @staticmethod
    def __dfa_simplify(dfa):
        '''Attempts to simplify the given DFA'''

        # Find any dead end states
        to_process = [dfa]
        done = set()
        dead_end = set()

        while to_process:
            # Pop and add to done list
            current = to_process.pop()
            done.add(current)

            # Dead-end?
            if (not current) and (not current.accepting):
                dead_end.add(current)

            else:
                # Add any sub states not processed yet
                for sub_state in current.itervalues():
                    if not sub_state in done:
                        to_process.append(sub_state)

        # Remove all the transitions to the dead end states
        for state in done:
            for key, value in dict(state).iteritems():
                if value in dead_end:
                    del state[key]

        return dfa

    def match(self, string):
        '''Returns true if the given input string matches this pattern'''
        pass

    def diagram(self):
        '''Returns a dot digraph of the compiled DFA'''

        result = "digraph {\n\tnode [shape=circle];\n"

        # Assign IDs to every state and print the list of accepted states
        to_process = [self.start]   # Stack of states
        id_dict = {}                # State -> generated id
        next_id = 1                 # Next id to use

        while to_process:
            # Get object
            state = to_process.pop()

            # Ignore if already processed
            if state in id_dict:
                continue

            # Give it an id
            id_dict[state] = next_id

            # Accepting?
            if state.accepting:
                # Add to result list
                result += ("\t" + str(next_id) + " [shape=doublecircle];\n")

            # Add all transitions to process stack
            to_process.extend(state.itervalues())
            next_id += 1

        # Print all the transitions
        result += "\n"
        for state, state_id in id_dict.iteritems():
            for char, other in state.iteritems():
                result += ("\t" + str(state_id) + " -> " + \
                           str(id_dict[other]) + ' [label="' + char + '"]\n')

        # Return result
        return result + "}\n"


if __name__ == '__main__':
    print Regex("ab(cd*e|d+)").diagram()
