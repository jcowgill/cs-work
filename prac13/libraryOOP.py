




class Member:
    """

    Class Member represents the library user entity. The class contains member's information
    and the list of borrowed items via their UID.


    """

    def __init__(self, firstname, surname, postcode, uid):
        """

        @param firstname is a string representing the user firstname
        @param surname is a string representing the user surname
        @param postcode is a string representing the user postcode
        @param uid is an object representing a unique identifier of a member

        """
        self.__firstname = firstname
        self.__surname = surname
        self.__postcode = postcode
        self.__uid = uid ## a unique identifier, no two members have the same
        self.__borrowed = [] ## a list of items uid

    def __repr__(self):
        return ('<Member: uid = ' + str(self.__uid) + ', ' + self.__surname + ', ' +
                str(self.__borrowed) + '>')

    def addItem(self, itemUID):
        """

        Add a new item to the list of borrowed item. The reference to the item
        is via its UID.

        @param itemUID is the unique identifier of an Item object
        @see Item
        """

        assert itemUID not in self.__borrowed
        self.__borrowed.append(itemUID)

    def getItems(self):
        return self.__borrowed

    def getUID(self):
        return self.__uid

    def getFirstname(self):
        return self.__firstname

    def getSurname(self):
        return self.__surname

    def removeItem(self, itemUID):
        assert itemUID in self.__borrowed
        self.__borrowed.remove(itemUID)


class Item:
    """

    Class Item represents the library Item entity. The class is intended to be
    subclassed. The class contains Item's title and UID. It also contains the
    UID of the member currently borrowing the item (Item.NO_BORROWER if no such
    member), and a status set to Item.AVAILABLE or Item.BORROWED.

    Class Constants:
        AVAILABLE   -- constant representing the status as available
        BORROWED    -- constant representing the status as borrowed
        NO_BORROWER -- constant representing the state that no member currently
                       owned the item

    """


    AVAILABLE = True
    BORROWED = False
    NO_BORROWER = None

    def __init__(self, title, uid):
        self.__title = title
        self.__uid = uid
        self.__status = Item.AVAILABLE  ## set to True if available, False otherwise
        self.__member = Item.NO_BORROWER ## If borrowed contains borrower member uid, None otherwise

    def __repr__(self):
        return ('<Item: uid = ' + str(self.__uid) + ', ' + self.__title +
                ', available = ' + str(self.__status) +
                ', borrower = ' + str(self.__member) + '>')

    def getMember(self):
        return self.__member

    def getStatus(self):
        return self.__status

    def getTitle(self):
        return self.__title

    def getUID(self):
        return self.__uid

    def isavailable(self):
        return self.__status == Item.AVAILABLE

    def setMember(self, memberID):
        self.__member = memberID

    def setStatus(self, newStatus):
        self.__status = newStatus




class Book(Item):

    def __init__(self, title, author, ISBN, uid):
        Item.__init__(self, title, uid)
        self.__author = author
        self.__ISBN = ISBN

    def __repr__(self):
        return ('<Book: uid = ' + str(self.getUID()) + ', ' + self.getTitle() +
                ', author = '+ self.__author +
                ', available = ' + str(self.getStatus()) +
                ', borrower = ' + str(self.getMember()) + '>')

    def getAuthor(self):
        return self.__author

    def getISBN(self):
        return self.__ISBN



class DVD(Item):

    def __init__(self, title, director, ageRating, uid):
        Item.__init__(self, title, uid)
        self.__director = director
        self.__ageRating = ageRating

    def __repr__(self):
        return ('<DVD: uid = ' + str(self.getUID()) + ', ' + self.getTitle() +
                ', director = '+ self.__director +
                ', available = ' + str(self.getStatus()) +
                ', borrower = ' + str(self.getMember()) + '>')

    def getDirector(self):
        return self.__director

    def getAgeRating(self):
        return self.__ageRating




class Library:

    def __init__(self):
        self.__members = {} ## Dictionary containing Member.uid as key and Member object as value
        self.__items = {} ## Dictionary containing Item.uid as key and Item object as value

    def __repr__(self):
        return ('<Library: \n member::'+ str(self.__members) + '\n items::' +str(self.__items) +'\n>')


    def add_item(self, item):
        '''
        creates and add an item to the library. Returns the item if it is created
        '''
        if self.containsItemUID(item.getUID()): ## uid already existing so must not add item
            raise DuplicateItemError("Item with id " + str(item.getUID()) + " already exists")
        else:
            self.__items[item.getUID()] = item
            return item


    def add_member(self, member):
        '''
        creates and add a member to the library. Returns the member if it is created
        '''
        if self.containsMemberUID(member.getUID()): ## uid already existing so must not add item
            raise DuplicateMemberError("Member with id " + str(member.getUID()) + " already exists")
        else:

            self.__members[member.getUID()] = member
            return member


    def borrow(self, item_uid, member_uid):
        '''
        returns True if an item has been successfully borrowed,
        False if the item has already been borrowed

        '''

        if not self.containsItemUID(item_uid):
            raise UnknownItemError("Item with id " + str(item_uid) + " does not exists")
        if not self.containsMemberUID(member_uid):
            raise UnknownMemberError("Member with id " + str(member_uid) + " does not exists")

        item = self.__items[item_uid]
        if item.isavailable():
            member = self.__members[member_uid]
            member.addItem(item_uid)
            item.setStatus(Item.BORROWED)
            item.setMember(member_uid)
            return True
        else: ## item is not available so cannot borrow
            return False

    def containsItemUID(self, uid):
        return (uid in self.__items.keys())


    def containsMemberUID(self, uid):
        return (uid in self.__members.keys())


    def return_item(self, item_uid):
        '''
        returns True if an item has been successfully returned,
        False otherwise if the item has not been borrowed
        '''

        if self.containsItemUID(item_uid):
            item = self.__items[item_uid]
            member_uid = item.getMember()
            if member_uid is None:
                return False

            elif not self.containsMemberUID(member_uid)):
                raise UnknownMemberError("Member with id " + str(member_uid) + " does not exists")

            else:
                member = self.__members[member_uid]
                assert item_uid in member.getItems()    #error, inconsistant data state between members and items

                member.removeItem(item_uid)
                item.setStatus(Item.AVAILABLE)
                item.setMember(Item.NO_BORROWER)
                return True

        else: # error item not in library
            raise UnknownItemError("Item with id " + str(item_uid) + " does not exists")



    def delete_item(self, item_uid):
        '''
        returns True if an item has be successfully deleted,
        False if item is still borrowed

        '''
        if(not self.containsItemUID(item_uid)): ## items not in library
            raise UnknownItemError("Item with id " + str(item_uid) + " does not exists")
        else:
            item = self.__items[item_uid]
            if (item.getMember() is not None): ## A member has item, so cannot delete it until returned.
                ## note it depends on the requirement of the program, could decide to remove item
                ## but in this case must remove it from the member list of borrowed items
                return False
            else:
                del self.__items[item_uid]
                return True


    def delete_member(self, member_uid):
        '''
        Returns True if a member with uid member_uid has been successfully deleted,
        False if member has still some borrowed items.
        Raise an UnknownMemberError if there is no member with uid member_uid in the libray.

        '''
        if(not self.containsMemberUID(member_uid)): ## member not in library
            raise UnknownMemberError("Member with id " + str(member_uid) + " does not exists")
        else:
            member = self.__members[member_uid]
            if (len(member.getItems()) > 0): ## member has items, so cannot delete it until all are returned.
                ## note it depends on the requirement of the program, could decide to remove member
                ## but in this case must remove all borrowed items from the library item's list.
                return False
            else:
                del self.__members[member_uid]
                return True

    def getMembers(self):
        return self.__members.values()


class DuplicateItemError(Exception):
    """Exception raised for duplicate items

    Attributes:
        msg  -- explanation of the error
    """

    pass

class DuplicateMemberError(Exception):
    """Exception raised for duplicate members

    Attributes:
        msg  -- explanation of the error
    """

    pass

class UnknownItemError(Exception):
    """Exception raised for errors in the Item UID, e.g. not an existing UID.

    Attributes:
        msg  -- explanation of the error
    """

    pass

class UnknownMemberError(Exception):
    """Exception raised for errors in the Member UID, e.g. not an existing UID.

    Attributes:
        msg  -- explanation of the error
    """

    pass
