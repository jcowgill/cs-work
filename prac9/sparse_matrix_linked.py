# Linked Sparse Matricies
#


class SparseMatrixLinked:
    '''Stores a sparse matrix using a list of linked lists'''

    class Node:
        '''A node in the linked list'''

        def __init__(self, x, value, next_node = None):
            '''Creates a new node with next pointing to nothing'''
            self.x = x
            self.value = value
            self.next = next_node

        def __repr__(self):
            return "<Node: x=" + str(self.x) + ",value=" + \
                   str(self.value) + ",next=" + repr(self.next) + ">"
    

    def __init__(self, width, height):
        '''Creates a new sparse matrix filled with zeros'''
        self.width = width
        self.height = height
        self.data = [None] * height

    def __repr__(self):
        return "<SparseMatrixLinked " + str(self.width) + \
                    "x" + str(self.height) + ">"

    def __check_range(self, key):
        '''Raises a ValueError if the given key is out of range'''
        if key[0] < 0 or key[0] >= self.width or \
            key[1] < 0 or key[1] >= self.height:

            raise ValueError("key is out of range")

    def __find_key(self, key):
        '''Validates and finds a key in the data structure

        Returns a tuple containing:
            [0] = The node (or None if it was not found)
            [1] = The previous node (or None if it was the first)
                  If [0] is None, this is the node that would have been before'''

        # Validate key
        self.__check_range(key)

        # Get initial node
        current = self.data[key[1]]
        previous = None

        # Loop until we've found the node
        ## while (key[0] < current.x) and (current.next is not None):
        while (current is not None) and (key[0] > current.x):
            previous = current
            current = current.next

        # Found?
        if (current is not None) and (key[0] != current.x):
            current = None

        return current, previous
    
    def __getitem__(self, key):
        # Get value or return 0
        find_result = self.__find_key(key)

        if find_result[0] is not None:
            return find_result[0].value
        else:
            return 0;

    def __setitem__(self, key, value):
        # Try to find existing value
        find_result = self.__find_key(key)

        # Did we find it?
        if find_result[0] is not None:
            # Removing or replacing value
            if value == 0:
                # Remove the link
                if find_result[1] is None:
                    self.data[key[1]] = find_result[0].next
                else:
                    find_result[1].next = find_result[0].next
            else:
                # Replace existing value
                find_result[0].value = value

        else:
            # Only add the value if it is not zero
            if value != 0:
                # Add new value after previous node
                if find_result[1] is None:
                    self.data[key[1]] = self.Node(key[0], value, self.data[key[1]])
                else:
                    find_result[1].next = self.Node(key[0], value, find_result[1].next)

    def sum(self, other):
        '''Sums this matrix with another and stores the result in place'''

        # Validate sizes
        if self.width != other.width or self.height != other.height:
            raise ValueError("matricies are not the same size")

        # New matrix and dummy node
        new_matrix = SparseMatrixLinked(self.width, self.height)
        head_node = self.Node(None, None)

        # Merge each linked list in each row
        for row_num in range(self.height):
            # Merge from right into left
            left = self.data[row_num]
            right = self.data[row_num]
            current = head_node

            # Process while both lists are avaliable
            while left is not None and right is not None:
                # Check both keys
                if left.x == right.x:
                    # Calculate new value
                    new_val = left.value + right.value

                    # Add value if it's not 0
                    if new_val != 0:
                        current.next = self.Node(left.x, new_val)
                        current = current.next
                        
                    left = left.next
                    right = right.next

                elif left.x < right.x:
                    # Add left
                    current.next = self.Node(left.x, left.value)
                    current = current.next
                    
                else:
                    # Add right
                    current.next = self.Node(right.x, right.value)
                    current = current.next

            # Add the list that's left
            while left is not None:
                current.next = self.Node(left.x, left.value)
                current = current.next
                left = left.next
                
            while right is not None:
                current.next = self.Node(right.x, right.value)
                current = current.next
                right = right.next

            # Add the new list to the matrix
            print current
            new_matrix.data[row_num] = head_node.next

        return new_matrix


a=SparseMatrixLinked(2,2)
b=SparseMatrixLinked(2,2)
a[0,0]=1
a[0,1]=2
b[1,0]=3
b[0,0]=55
