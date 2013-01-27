# Binary Search Tree
#  James Cowgill
#


class BinaryTree:
    '''A dictionary implemented using an unbalanced binary search tree'''

    class _Node:
        '''A node in the tree'''

        def __init__(self, key, value, parent):
            '''Creates a new leaf node'''
            self.key = key
            self.value = value
            self.parent = parent
            self.left = None
            self.right = None

        def minimum(self):
            '''Returns the left most item in this subtree'''
            current = self
            while current.left is not None:
                current = current.left

            return current

        def successor(self):
            '''Returns the successor of this node or None'''
            if self.right is not None:
                # Find left most node of right subtree
                return self.right.minimum()

            else:
                # Go up the tree until we are not the right child
                current = self
                parent = self.parent

                while (parent is not None) and (current == parent.right):
                    current = parent
                    parent = parent.parent

                return parent

    # Public Viewing Methods
    # ============================
    def __init__(self):
        '''Creates a new dictionary'''
        self.clear()

    def clear(self):
        '''Removes all items from the dictionary'''
        self.__root = None
        self.__size = 0

    def __contains__(self, key):
        '''Returns true if the dictionary contains the given key'''
        return self.__find_node(key)[0]

    def __getitem__(self, key):
        '''Gets the item indexed by the given key'''
        result = self.__find_node(key)

        if result[0]:
            return result[1].value
        else:
            raise KeyError(key)

    def __iter__(self):
        '''In-order traversal'''
        if self.__root is not None:
            current = self.__root.minimum()

            while current is not None:
                yield (current.key, current.value)
                current = current.successor()

    def __len__(self):
        '''Returns the number of items in the dictionary'''
        return self.__size

    def __repr__(self):
        return "<AvlDictionary: " + ', '.join([repr(x) for x in self]) + ">"

    def get(self, key):
        '''Gets the item indexed by the given key
        or returns None if the key was not found'''
        result = self.__find_node(key)

        if result[0]:
            return result[1].value
        else:
            return None

    # Public Mutating Methods
    # ============================
    def __delitem__(self, key):
        '''Removes the given key from the dictionary'''

        # Find node to delete
        result = self.__find_node(key)

        if not result[0]:
            # Not found
            raise KeyError(key)

        # How many children?
        current = result[1]
        if (current.left is not None) and (current.right is not None):
            # Both - get successor
            successor = current.right.minimum()

            # Overwrite value
            current.key = successor.key
            current.value = successor.value

            # Delete successor
            self.__delete_update_parent(successor, successor.right)

        elif current.left is not None:
            # Left only
            self.__delete_update_parent(current, current.left)

        elif current.right is not None:
            # Right only
            self.__delete_update_parent(current, current.right)

        else:
            # Leaf node
            self.__delete_update_parent(current, None)

    def __setitem__(self, key, value):
        '''Sets an item in the dictionary'''

        # Find place to insert
        result = self.__find_node(key)

        if result[0]:
            # Replace existing value
            result[1].value = value

        else:
            # Insert new node as a child of result[1]
            parent = result[1]
            new_node = BinaryTree._Node(key, value, parent)
            self.__size += 1

            # Root insert?
            if parent is None:
                self.__root = new_node

            elif key < parent.key:
                parent.left = new_node

            else:
                parent.right = new_node

    # Private Methods
    # ============================
    def __find_node(self, key):
        '''Gets the node belonging to the given key

        returns a tuple (bool, node)
         if the node was found, the bool is true and the node is returned
         if not, the bool is false and its parent is returned'''

        parent = None
        current = self.__root

        while current is not None:
            parent = current

            if key == current.key:
                # Found the node
                return (True, current)

            elif key < current.key:
                # Move left
                current = current.left

            else:
                # Move right
                current = current.right

        # Not found
        return (False, parent)

    def __delete_update_parent(self, deleted, new_node):
        '''Updates the left / right pointer in deleted.parent to new_node'''
        parent = deleted.parent

        if parent is None:
            # Update root pointer
            self.__root = new_node

        elif deleted.key < parent.key:
            # Update left pointer
            parent.left = new_node

        else:
            # Update right pointer
            parent.right = new_node

        # Update new_node's parent
        if new_node is not None:
            new_node.parent = parent
