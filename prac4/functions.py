# Practical 4 Functions
#

# Sum a list (probably don't want me to chat this way)
sumList = sum

def squareEach(nums):
    '''Squares each of the input numbers'''
    return [x * x for x in nums]

def toNumbers(str_list):
    '''Converts space separted number to a list of numbers'''
    return [float(x) for x in str_list.split()]

# Conversion class
class Converter:
    '''Provides imperial to metric conversion functions'''

    def __init__(self, scale_factor, conversion_factor):
        '''Creates a new conversion factor

        scale_factor = how many times bigger the first item is compared to the second
        conversion_factor = the conversion factor for small -> metric'''
        
        self.scale_factor = scale_factor
        self.conversion_factor = conversion_factor

    def __repr__(self):
        return "Converter(" + str(self.scale_factor) + ", " + str(self.conversion_factor) + ")"

    def to_metric(self, imperial):
        '''Converts the given tuple to metric'''
        item1, item2 = imperial
        return (item1 * self.scale_factor + item2) * self.conversion_factor

    def to_imperial(self, metric):
        '''Converts the given item to an imperial tuple'''
        total = metric / self.conversion_factor
        return (total // self.scale_factor, total % self.scale_factor)

# Imperial conversion functions
length_small = Converter(12, 0.0254)
length_large = Converter(1760, 0.914)
capacity = Converter(8, 0.568)
mass = Converter(14, 0.453)
