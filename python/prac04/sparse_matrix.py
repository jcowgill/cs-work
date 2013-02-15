# Sparse Matricies
#


class SparseMatrix:
    '''Stores a sparse matrix'''

    def __init__(self, width, height):
        '''Creates a new sparse matrix filled with zeros'''
        self.width = width
        self.height = height
        self.data = {}

    @staticmethod
    def from_2d_list(data):
        '''Builds a sparse matrix from a 2D list'''
        height = len(data)

        # Check empty lists
        if height == 0:
            return SparseMatrix(0, 0)

        width = len(data[0])
        matrix = SparseMatrix(width, height)

        # Process the lists
        for row in range(height):
            for col in range(width):
                if data[row][col] != 0:
                    matrix.data[col, row] = data[row][col]

        return matrix

    def __repr__(self):
        return "<SparseMatrix " + str(self.width) + \
                    "x" + str(self.height) + ">"

    def __check_range(self, key):
        '''Raises a ValueError if the given key is out of range'''
        if key[0] < 0 or key[0] >= self.width or \
            key[1] < 0 or key[1] >= self.height:

            raise ValueError("key is out of range")

    def __getitem__(self, key):
        # Validate key
        self.__check_range(key)

        # Get value
        if key in self.data:
            return self.data[key]
        else:
            return 0

    def __setitem__(self, key, value):
        # Validate key
        self.__check_range(key)

        # If the new value is 0, delete the key instead of replacing it
        if value != 0:
            self.data[key] = value

        elif key in self.data:
            del self.data[key]

    def copy(self):
        '''Creates a copy of the sparse matrix'''
        new = SparseMatrix(self.width, self.height)
        new.data = self.data.copy()
        return new

    def resize(self, width, height):
        '''Resizes the matrix - extra areas are filled with zeros'''

        # Are we getting smaller?
        if width < self.width or height < self.height:
            if width == 0 or height == 0:
                # Erase everything
                self.data = {}

            else:
                # Cull any entries out of range
                for key in self.data.keys():
                    if key[0] >= width or key[1] >= height:
                        del self.data[key]

        # Set new dimensions
        self.height = height
        self.width = width

    def sum_in_place(self, other):
        '''Sums this matrix with another and stores the result in place'''

        # Validate sizes
        if self.width != other.width or self.height != other.height:
            raise ValueError("matricies are not the same size")

        # Do the addition
        for key, value in other.data.items():
            self[key] += value

        return self

    def sum(self, other):
        '''Sums this matrix with another and returns a new matrix'''

        return self.copy().sum_in_place(other)

    def transpose(self):
        '''Returns a new matrix which is the transpose of this one'''

        new = SparseMatrix(self.width, self.height)

        for key, value in self.data.items():
            new.data[key[1], key[0]] = value

        return new

    def to_2d_list(self):
        '''Converts this matrix to a 2d list'''
        new = []

        for row in range(self.height):
            row_data = []

            for col in range(self.width):
                row_data.append(self[col, row])

            new.append(row_data)

        return new
