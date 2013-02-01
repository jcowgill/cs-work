# Object Oriented version of the Library program (practical 5)
#  James Cowgill
#


class AvailabilityError(Exception):
    '''Error raised if an item is unavailable or borrowed by another member'''
    pass


class Item:
    '''Abstract class representing an item in the library'''

    def __init__(self, item_id, title):
        '''Creates a new Book object which is available for taking out'''

        self.__item_id = item_id
        self.__title = title
        self.__member = None

    def __repr__(self):
        return "Item(" + repr(self.__item_id) + ")"

    def is_available(self):
        '''Returns true if the book is available for taking out'''
        return self.__member is None

    def get_title(self):
        return self.__title

    def get_id(self):
        return self.__item_id

    def get_member(self):
        return self.__member

    def set_member(self, value):
        self.__member = value


class Book(Item):
    '''Class representing Book items'''

    def __init__(self, item_id, title, author, isbn):
        '''Creates a new Book object which is available for taking out'''

        Item.__init__(self, item_id, title)
        self.__author = author
        self.__isbn = isbn

    def __repr__(self):
        return "Book(" + repr(self.get_id()) + ", " + repr(self.get_title()) + \
                  ", " + repr(self.__author) + ", " + repr(self.__isbn) + ")"

    def get_author(self):
        return self.__author

    def get_isbn(self):
        return self.__isbn


class Dvd(Item):
    '''Class representing DVD items'''

    def __init__(self, item_id, title, director):
        '''Creates a new Dvd object which is available for taking out'''

        Item.__init__(self, item_id, title)
        self.__director = director

    def __repr__(self):
        return "Dvd(" + repr(self.get_id()) + ", " + repr(self.get_title()) + \
                  ", " + repr(self.__director) + ")"

    def get_director(self):
        return self.__director


class Member:
    '''Class representing a member of the library'''

    def __init__(self, member_id, name, postcode):
        '''Creates a new Member who has taken out no items'''

        self.__member_id = member_id
        self.__name = name
        self.__postcode = postcode
        self.__items = set()

    def __repr__(self):
        return "Member(" + repr(self.__member_id) + ", " + \
                           repr(self.__name) + ", " + \
                           repr(self.__postcode) + ")"

    def get_id(self):
        return self.__item_id

    def get_name(self):
        return self.__name

    def get_postcode(self):
        return self.__postcode

    def get_items(self):
        return self.__items

    def print_items(self):
        '''Prints the list of items borrowed by this member'''
        for item in self.__items:
            print item


class Library:
    '''Main library class which stores the collection of members and items'''

    def __init__(self):
        self.__members = {}
        self.__next_member_id = 1
        self.__items = {}
        self.__next_item_id = 1

    def __repr__(self):
        return "<Library: " + str(len(self.__members)) + " members, " + \
                              str(len(self.__items)) + " items>"

    def add_member(self, name, postcode):
        '''Adds a new member to the library and returns its member id'''

        # Get new member id
        member_id = self.__next_member_id
        self.__next_member_id += 1

        # Add member
        self.__members[member_id] = Member(member_id, name, postcode)

        return member_id

    def get_member(self, member_id):
        '''Gets a member object from an id'''
        return self.__members[member_id]

    def remove_member(self, member_id):
        '''Removes a member - ensuring all their items are returned'''

        # Lookup member
        member_obj = self.__members[member_id]

        # Return any items
        for item in member_obj.get_items():
            assert item.get_member() == self
            item.set_member(None)

        member_obj.get_items().clear()

        # Remove from list of members
        del self.__members[member_id]

    def add_book(self, title, author, isbn):
        '''Adds a new book to the library and returns its item id'''

        # Get new item id
        item_id = self.__next_item_id
        self.__next_item_id += 1

        # Add item
        self.__items[item_id] = Book(item_id, title, author, isbn)
        return item_id

    def get_item(self, item_id):
        '''Gets an item object from an id'''
        return self.__items[item_id]

    def remove_item(self, item_id):
        '''Removes an item - you cannot remove an item that is not available'''

        # Lookup item
        item_obj = self.__items[item_id]

        # Ensure item is available
        if not item_obj.is_available():
            raise AvailabilityError("cannot remove unavailable item")

        # Remove from list
        del self.__items[item_id]

    def borrow_item(self, member_id, item_id):
        '''Borrows an item for the given member'''

        # Lookup member and item
        member_obj = self.__members[member_id]
        item_obj = self.__items[item_id]

        # Ensure item is available
        if not item_obj.is_available():
            if item_obj.get_member() is not member_obj:
                raise AvailabilityError("cannot borrow unavailable item")
            else:
                return  # Already borrowed this item

        # Borrow item
        member_obj.get_items().add(item_obj)
        item_obj.set_member(member_obj)

    def return_item(self, member_id, item_id):
        '''Returns an item for the given member'''

        # Lookup member and item
        member_obj = self.__members[member_id]
        item_obj = self.__items[item_id]

        # Ensure item was taken out by that member
        if item_obj.get_member() is not member_obj:
            raise AvailabilityError("item is not owned by this member")

        # Return item
        member_obj.get_items().remove(item_obj)
        item_obj.set_member(None)

    def search_items(self, title):
        '''Returns a list of all item ids of the items with the given title'''
        lst = []

        for key in self.__items:
            if self.__items[key].get_title() == title:
                lst.append(key)

        return lst

    def search_members(self, name):
        '''Returns a list of all member ids of the members with the given name'''
        lst = []

        for key in self.__members:
            if self.__members[key].get_name() == name:
                lst.append(key)

        return lst

if __name__ == '__main__':
    # Some tests
    library = Library()
    b1 = library.add_book("Book of Death", "Yourself", "42")
    m1 = library.add_member(("Joe", "Bloggs"), "SANTA")
    library.borrow_item(m1, b1)
    library.borrow_item(m1, b1)

    library.get_member(m1).print_items()
