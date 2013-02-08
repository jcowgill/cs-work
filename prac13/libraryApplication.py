from libraryOOP import *

main_library = Library()


def addNewMember(library):
    while True:
        print "\n\n\n"
        fname = raw_input('Enter member\'s firstname: ')
        sname = raw_input('Enter member\'s surname: ')
        postcode = raw_input('Enter member\'s postcode: ')
        member = Member(fname, sname, postcode, getUniqueIdentifier('usr_'))
        success = library.add_member(member)
        if success:
            print 'member added successfully.'
            return
        else:
            again = raw_input('An error has occurred, do you want to try again (y/n)? ')
            if again.lower() != 'y':
                return

def addNewDVD(library):
    while True:
        print "\n\n\n"
        title = raw_input('Enter DVD\'s title: ')
        director = raw_input('Enter DVD\'s Director: ')
        rating = raw_input('Enter DVD\'s age rating: ')
        dvd = DVD(title, director, rating, getUniqueIdentifier('dvd_'))
        success = library.add_item(dvd)
        if success:
            print 'Item added successfully.'
            return
        else:
            again = raw_input('An error has occurred, do you want to try again (y/n)? ')
            if again.lower() != 'y':
                return

def addNewItem(library):
    while True:
        print "\n\n\n"
        print "Select the type of item to add:"
        print "\t 1 - DVD"
        print "\t 2 - Book"
        print "\t 0 - Exit"

        type_item = raw_input("Enter your choice: ")

        if (type_item == '1'):
            addNewDVD(library)
            break

        elif  (type_item == '2'):
            pass

        elif (type_item == '0'): #exit the menu
            break

        else:
            print "The choice you made was not recognised"
            continue # restart at the begining of the iteration for another input



UID_base = 1
UID_shift = 1000000
def getUniqueIdentifier(prefix):
    global UID_base
    UID_base += 1
    return str(prefix)+str(UID_shift + UID_base)
    return


def printMembers(library):
    print '\n\n\t      ------------------     \n'
    print '\t Library\'s member list:\n'
    for member in library.getMembers():
        displayMember(member)

    print '\n\t      ------------------     \n\n'

def displayMember(member):
    assert isinstance(member, Member)
    print '\t[', member.getUID(), '] name: ', member.getFirstname(), member.getSurname(),
    print ',  borrowed item(s):', len(member.getItems())


def main():
    while True:
        print "Select the type of item to rent:"
        print "\t 1 - Return Item"
        print "\t 2 - Borrow Item"
        print "\t 3 - Add New Item"
        print "\t 4 - Add New Member"
        print "\t 5 - Delete Member"
        print "\t 6 - Delete Item"
        print "\t 7 - Print all members"
        print "\t 0 - Exit"

        type_item = raw_input("Enter your choice: ")

        if (type_item == '1'):
            pass
        elif  (type_item == '2'):
            pass

        elif  (type_item == '3'):
            addNewItem(main_library)

        elif  (type_item == '4'):
            addNewMember(main_library)

        elif  (type_item == '5'):
            pass

        elif  (type_item == '6'):
            pass

        elif  (type_item == '7'):
            printMembers(main_library)

        elif (type_item == '0'): #exit the menu
            break

        else:
            print "The choice you made was not recognised"
            continue # restart at the begining of the iteration for another input

if __name__ == '__main__':
    main()
