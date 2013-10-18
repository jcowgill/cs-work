# Linked List practical
# James Cowgill
#


class Node:
    def __init__(self, element):
        self.element = element
        self.next = None

    def __repr__(self):
        # # This method is recursive, you could use a while loop instead
        rep = '<Node: '
        rep += str(self.element)
        if self.next != None:
            # # the following statement is the recursive call. <str(self.next)> calls
            # # the __repr__ of the next node, hence the recursive nature of this method.
            rep += ', ' + str(self.next)

        rep += '>'
        return rep


class LinkedList:
    def __init__(self):
        self.head = None

    def __repr__(self):
        rep = '<LinkedList: '
        if self.head != None:
            rep += ', ' + str(self.head)

        rep += '>'
        return rep


def isempty(linkedList):
    return linkedList.head == None


def printLinkedListNonRecursive(linkedList):
    currentNode = linkedList.head
    while currentNode != None:
        print currentNode.element
        currentNode = currentNode .next


def printNodeRecursive(node):
    if node == None:
        return
    else:
        print node.element
        printNodeRecursive(node.next)


def printLinkedListRecursive(linkedList):
    printNodeRecursive(linkedList.head)

# # MY CODE BELOW HERE
#####################################


def add_first(lst, element):
    '''Adds a new element as the first item in the list'''
    node = Node(element)
    node.next = lst.head
    lst.head = node


def size(lst):
    '''Gets the size of the list iteratively'''
    current = lst.head
    total = 0

    # Go through the list counting the elements
    while current is not None:
        total += 1
        current = current.next

    return total


def _size_node(node):
    '''Recursive helper for size_recur'''
    if node is None:
        return 0
    else:
        return 1 + _size_node(node.next)


def size_recur(lst):
    '''Gets the size of the list recursively'''
    return _size_node(lst.head)

    # Question 2: You could add a size variable to the linked list to keep track
    #  of the size. It will be updated whenever something is added or deleted


def add_last(lst, element):
    '''Adds a new element as the last item in the list

    Note this function is O(n)'''

    new_node = Node(element)

    # Adding at the start?
    if isempty(lst):
        lst.head = new_node

    else:
        # Go to the end of the list and add there
        current = lst.head
        while current.next is not None:
            current = current.next

        current.next = new_node


def insert_at(lst, element, pos):
    '''Inserts an element before the given position in the list

    pos must be a number >= 0 and < list size
    Note this function is O(pos)'''

    if pos < 0:
        raise ValueError("pos must be greater or equal to 0")

    # Handle special case of pos == 0
    if pos == 0:
        add_first(lst, element)

    elif isempty(lst):
        # List is empty but pos != 0, so this is an error
        raise ValueError("pos is beyond the end of the list")

    else:
        # Go thorugh until we are at the right position
        current = lst.head
        for _ in range(pos - 1):
            # Advance pointer
            current = current.next

            # Check for end of list
            if current is None:
                raise ValueError("pos is beyond the end of the list")

        # Add at this position
        new_node = Node(element)
        new_node.next = current.next
        current.next = new_node


def remove_first(lst):
    '''Removes the first element of the list and returns it

    Returns None if there are no elements in the list'''

    removed = lst.head

    if removed is None:
        return None
    else:
        lst.head = removed.next
        return removed.element


def remove_last(lst):
    '''Removes the last element of the list and returns it

    Returns None if there are no elements in the list
    Note this function is O(n)'''

    # Check for special cases
    if isempty(lst):
        return None

    elif lst.head.next is None:
        # Last element is the only element
        removed = lst.head
        lst.head = None

    else:
        # Go to the penultimate element and remove the element after that
        previous = lst.head
        current = previous.next

        while current.next is not None:
            previous = current
            current = current.next

        # previous is now the element before the one we are going to remove
        removed = current
        previous.next = None

    return removed.element


def remove_at(lst, pos):
    '''Removes the last element of the list and returns it

    pos must be a number >= 0 and < list size
    Note this function is O(pos)'''

    # Check for special cases
    if pos < 0:
        raise ValueError("pos must be greater or equal to 0")

    elif isempty(lst):
        raise ValueError("pos is beyond the end of the list")

    elif pos == 0:
        # Remove first item
        return remove_first(lst)

    else:
        # Go to the element before the one we want to remove
        current = lst.head
        for _ in range(pos - 1):
            # Advance pointer
            current = current.next

            # Check for end of list
            if current is None:
                raise ValueError("pos is beyond the end of the list")

        # Remove the next element
        removed = current.next
        if removed is None:
            raise ValueError("pos is beyond the end of the list")
        current.next = removed.next

        return removed.element


def reverse(lst):
    '''Returns a new linked list which is the reverse of the given list'''
    new_lst = LinkedList()
    current = lst.head

    while current is not None:
        # Add this item to the front of the new list
        add_first(new_lst, current.element)

        # Advance to next item
        current = current.next

    return new_lst


# Test list
lst = LinkedList()
add_first(lst, "c")
add_first(lst, "b")
add_first(lst, "a")

add_last(lst, "last")
add_last(lst, "last2")
add_first(lst, "first")
insert_at(lst, "0", 0)
insert_at(lst, "5", 5)
insert_at(lst, "7", 7)
insert_at(lst, "9", 9)
