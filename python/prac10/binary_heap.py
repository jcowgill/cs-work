# Priority Queue using a Binary Heap
#  James Cowgill
#


class BinaryHeap:
    '''A structure which stores items which can be removed in order of
    priority'''

    def __init__(self, lst=[]):
        '''Creates a new BinaryHeap using the data in the given list
        The list must contain tuples of (priority, data) and priority must be
        numeric'''

        # Copy the list
        self.__data = lst[:]

        # Build the heap
        for index in reversed(range(len(lst) // 2)):
            self.__bubble_down(index)

    def __repr__(self):
        return "BinaryHeap(" + repr(self.__data) + ")"

    def __len__(self):
        '''Returns the number of items in the heap'''
        return len(self.__data)

    def insert(self, priority, data):
        '''Inserts a new item into the queue'''

        # Insert as last item in the array
        self.__data.append((priority, data))

        # Bubble up to correct position
        self.__bubble_up(len(self.__data) - 1)

    def get_min(self):
        '''Returns the item with the minimum priority

        This returns a tuple of (priority, data)'''

        if self.__data:
            return self.__data[0]
        else:
            raise KeyError()

    def remove_min(self):
        '''Removes and returns the item with the minimum priority

        This returns a tuple of (priority, data)'''

        # Swap first with last and remove last node
        if not self.__data:
            raise KeyError()

        self.__swap_nodes(0, len(self.__data) - 1)
        smallest = self.__data.pop()

        # Bubble root node down to correct position
        self.__bubble_down(0)

        return smallest

    def __bubble_up(self, index):
        '''Bubbles the given node upwards'''
        priority = self.__data[index][0]

        while index > 0 and self.__data[self.__parent(index)][0] > priority:
            parent_index = self.__parent(index)
            self.__swap_nodes(index, parent_index)
            index = parent_index

    def __bubble_down(self, index):
        '''Bubbles the given node downwards'''

        while True:
            # Are any of this node's children smaller than this node?
            left_index = self.__left(index)
            right_index = self.__right(index)
            smallest = index

            if left_index < len(self.__data):
                if self.__data[left_index][0] < self.__data[index][0]:
                    smallest = left_index

                if right_index < len(self.__data) and \
                   self.__data[right_index][0] < self.__data[smallest][0]:
                    smallest = right_index

            # Finish if they're already in the correct order
            if index == smallest:
                break

            # Swap with the smallest child and repeat
            self.__swap_nodes(index, smallest)
            index = smallest

    def __swap_nodes(self, index1, index2):
        '''Swaps the nodes at the given indexes'''
        self.__data[index1], self.__data[index2] = \
            self.__data[index2], self.__data[index1]

    def __parent(self, index):
        '''Returns the parent of the given node index'''
        return (index - 1) // 2

    def __left(self, index):
        '''Returns the left child of the given node index'''
        return 2 * index + 1

    def __right(self, index):
        '''Returns the right child of the given node index'''
        return 2 * index + 2

