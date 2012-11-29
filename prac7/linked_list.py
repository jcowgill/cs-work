# Linked List practical
# James Cowgill
#


class Node:
    def __init__(self, element):
        self.element = element
        self.next = None

    def __repr__(self):
        ## This method is recursive, you could use a while loop instead
        rep = '<Node: '
        rep += str(self.element)
        if self.next != None:
            ## the following statement is the recursive call. <str(self.next)> calls
            ## the __repr__ of the next node, hence the recursive nature of this method.
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
