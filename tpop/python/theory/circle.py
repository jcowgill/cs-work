# Holding hands in a circle problem
# James Cowgill
#


class Node:
    '''A node in the linked list'''

    def __init__(self, value, nxt=None):
        self.value = value
        self.nxt = nxt

    def __repr__(self):
        result = "<Node " + str(self.value)
        if not self.nxt is None:
            result += ", nxt = " + str(self.nxt.value)

        return result + ">"


def do_circle(n, m):
    '''Does the problem and returns the list of eliminations'''
    final_list = []

    # Create list
    last_node = Node(n)
    current_node = last_node

    for i in range(n - 1, 0, -1):
        current_node = Node(i, current_node)

    # Join last node to make the circle
    last_node.nxt = current_node
    prev_node = last_node

    # Do the eliminations
    while current_node.nxt != current_node:
        # Move m-1 times
        for i in range(m - 1):
            prev_node = current_node
            current_node = current_node.nxt

        # Eliminate this node
        prev_node.nxt = current_node.nxt
        final_list.append(current_node.value)

        # Advance to a proper node
        current_node = current_node.nxt

    # Eliminate final node
    final_list.append(current_node.value)
    return final_list
