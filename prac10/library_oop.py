# Object Oriented version of the Library program (practical 5)
#  James Cowgill
#

class Book:
    '''Class representing Book items'''

    def __init__(self, item_id, title, author, isbn):
        '''Creates a new Book object which is avaliable for taking out'''
        
        self.__item_id = item_id
        self.__title = title
        self.__author = author
        self.__isbn = isbn
        self.__member = None

    def __repr__(self):
        return "Book(" + repr(self.__item_id) + ", " + repr(self.__title) + ", " + \
                         repr(self.__author) + ", " + repr(self.__isbn) + ")"

    def is_avaliable(self):
        '''Returns true if the book is avaliable for taking out'''
        return self.__member is None

    def get_id(self):
        return self.__item_id
    
    def get_title(self):
        return self.__title

    def get_author(self):
        return self.__author

    def get_isbn(self):
        return self.__isbn

    def get_member(self):
        return self.__member

    def set_member(self, value):
        '''Sets this item's member. Only the Member class may call this.'''
        self.__member = value

    def return_item(self):
        '''Returns this item - making it avaliable again'''
        if self.__member is not None:
            self.__member.return_item(self)


class Member:
    '''Class representing a member of the library'''

    def __init__(self, member_id, name, postcode):
        '''Creates a new Member who has taken out no items'''
        
        self.__member_id = member_id
        self.__name = name
        self.__postcode = postcode
        self.__items = set()

    def __repr__(self):
        return "Member(" + repr(self.__member_id) + ", " + repr(self.__name) + ", " + \
                         repr(self.__postcode) + ")"

    def get_id(self):
        return self.__item_id

    def get_name(self):
        return self.__name
    
    def get_postcode(self):
        return self.__postcode

    def get_items(self):
        return frozenset(self.__items)

    def take_out(self, item):
        '''Causes this member to take out the given item'''
        if item.is_avaliable():
            item.set_member(self)
            self.__items.add(item)

        else:
            # DO SOMETHING HERE #################################################
            pass
        
    def return_item(self, item):
        '''Returns the given item'''
        if item.get_member() is self:
            item.set_member(None)
            self.__items.remove(item)

        else:
            # DO SOMETHING HERE #################################################
            pass

    def return_all(self):
        '''Returns all the items this member owns'''
        for item in self.get_items():
            self.return_item(item)

    def print_items(self):
        '''Prints the list of items borrowed by this member'''
        for item in self.__items:
            print item
        
