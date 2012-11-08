# Sparse Matricies
#

class SparseMatrix:
    '''Stores a sparse matrix'''

    def __init__(self, width, height):
        '''Creates a new sparse matrix filled with zeros'''
        self.width = width
        self.height = height
        self.data = {}

    def __repr__(self):
        return "<SparseMatrix " + str(self.width) + "x" + str(self.height) + ">"

    def __getitem__(self, key):
        # Validate range
        
        
        # Get value
        if key in self.data:
            return self.data[key]
        else:
            return 0

    def __setitem__(self, key, value):
        self.data[key] = value
