# TPOP Practical 5
# Mini Assessment
#  James Cowgill
#


class Member:
    '''Represents a member of the library'''

    def __init__(self, fname, sname, postcode):
        '''Creates a new member who has taken out no items'''
        self.fname = fname
        self.sname = sname
        self.postcode = postcode
        self.items = []

    def __repr__(self):
        return '<Member "' + self.fname + ' ' + self.sname + '">'
