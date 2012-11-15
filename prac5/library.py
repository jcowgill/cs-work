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
        self.items = set()

    def __repr__(self):
        return '<Member "' + self.fname + ' ' + self.sname + '">'

    def key(self):
        '''Returns the key for this member (like member_get_key)'''
        return member_get_key(self.fname, self.sname)


class Media:
    '''Enumeration of types of media'''
    DVD = 0
    CD = 1
    GAME = 2
    BOOK = 3


class Item:
    '''Represents an item in the library'''

    def __init__(self, item_id, title, author, media):
        '''Creates a new item which has not been taken out'''

        self.item_id = item_id
        self.title = title
        self.author = author
        self.media = media
        self.member = None

    def __repr__(self):
        return '<Item ' + str(self.item_id) + ' "' + self.title + '">'


# Global dictionaries
members = {}        # Stores Member objects by (fname, sname) tuple
                    #  Member names stored here are in LOWER CASE!!
items = {}          # Stores Item objects by item_id
next_item_id = 1    # The next avaliable item id


# Member key tuple
def member_get_key(fname, sname):
    '''Returns the key used in the members dictionary for the given names'''
    return (fname.lower(), sname.lower())


# Adding data
def member_add(fname, sname, postcode):
    '''Creates a new member and adds it to the global member list

    Returns the new object or None of the member already exists'''

    # Create key
    member_key = member_get_key(fname, sname)

    # Does that member exist?
    if member_key in members:
        return None

    # Create and add member
    member_obj = Member(fname, sname, postcode)
    members[member_key] = member_obj
    return member_obj


def item_add(title, author, media, item_id=-1):
    '''Creates a new item and adds it to the global item list

    item_id is optional, if it is left out a new id will be generated

    Returns the new object or None if the item already exists'''

    global next_id

    # Auto-generating item?
    if item_id == -1:
        item_id = next_item_id

    # Does that item exist?
    if item_id in items:
        return None

    # Create and add item
    item_obj = Item(item_id, title, author, media)
    items[item_id] = item_obj

    # Update next id (if nessesary)
    if item_id >= next_id:
        next_item_id = next_item_id + 1

    return item_obj


# Removing data
def member_delete(member_key):
    '''Deletes the member with the given key

    Returns True on success, False if that member does not exist'''

    if member_key in members:
        del members[member_key]
        return True
    else:
        return False


def item_delete(item_id):
    '''Deletes the item with the given id

    Returns True on success, False if that member does not exist'''

    if item_id in items:
        del items[item_id]
        return True
    else:
        return False


# Borrowing and returning items
def borrow_item(item_obj, member_obj):
    '''Tries to mark the given item as borrowed by the given member

    Returns True on success, False if the item is already taken out'''

    if item_obj.member is None:
        # Add item to set and save member
        member_obj.items.add(item_obj)
        item_obj.member = member_obj
        return True

    else:
        return False


def return_item(item_obj, member_obj):
    '''Tries to mark the given item as returned by the given member

    Returns True on success, False if the item has not been taken out
    by that member'''

    if item_obj.member == member_obj:
        # Remove item from set and mark as avaliable
        member_obj.items.remove(item_obj)
        item_obj.member = None
        return True

    else:
        return False


def print_items(member_obj):
    '''Prints the list of items taken out by a member'''

    for item in member_obj.items:
        print item
