# Product Menu
#  Version 2


class Bill:
    '''Mutable class storing the current bill'''

    def __init__(self):
        self.items = []

    def __len__(self):
        return len(self.items)

    def add(self, item):
        '''Adds a product to the bill'''
        self.items.append(item)

    def remove(self, number):
        '''Removes the given item on the bill'''
        del self.items[number - 1]

    def total(self):
        '''Returns the total price of the bill'''
        current_total = 0.0

        for product in self.items:
            current_total += product.price

        return current_total

    def display(self):
        '''Prints the bill to the user'''
        print "Current Bill"
        print "---------------"

        for num, product in enumerate(self.items):
            print "{0:2}) {1!s:20}{1.price:5.2f}".format(num + 1, product)

        print " Total = {:20.2f}".format(self.total())
        print


class Product:
    '''A menu item which represents a product'''

    def __init__(self, name, price):
        '''Creates a new product with the given name and price'''
        self.name = name
        self.price = price

    def __str__(self):
        return self.name

    def process(self, bill):
        '''Called to process this selection'''
        bill.add(self)


class Menu:
    '''A menu item which leads to another menu'''

    def __init__(self, name, items):
        '''Creates a new menu with the given name and list of items'''
        self.name = name
        self.items = items

    def __str__(self):
        return self.name

    def process(self, bill):
        '''Called to process this selection'''

        while True:
            # Display menu
            print self.name
            print "------------"
            for i, item in enumerate(self.items):
                print "{0:2}) {1!s}".format(i + 1, item)

            print " M) Main Menu"

            # Get selection
            selection = raw_input("Enter selection: ")
            print

            if selection.isdigit():
                # Lookup number in items list
                item_number = int(selection) - 1
                if item_number >= 0 and item_number < len(self.items):
                    # Process that item and finish
                    self.items[item_number].process(bill)
                    break

            elif selection.upper() == "M":
                break


class RootMenu(Menu):
    '''The root menu'''

    # In this menu self.items actually contains the list of items for the
    # ADD option. The other options are handled by the overridden process
    # method

    def __init__(self, items):
        '''Creates a new main menu with the given items'''
        Menu.__init__(self, "Add an item", items)

    def __str__(self):
        return "Main Menu"

    def process(self, bill):
        '''Processes the menu system

        Unlike other process functions, this handles bill control and will not
        return until the user has selected exit
        '''

        while True:
            # Print current bill
            print '======================================='
            bill.display()

            # Print menu
            print "Main Menu"
            print "------------"
            print " Enter the number of an item on the bill to remove it"
            print " A) Add an item"
            print " X) Exit"

            # Get selection
            selection = raw_input("Enter selection: ").upper()
            print

            if selection.isdigit():
                # Remove given item
                number = int(selection)
                if number > 0 and number <= len(bill):
                    bill.remove(int(number))

            elif selection == 'A':
                # Add an item = process sub menu
                Menu.process(self, bill)

            elif selection == 'X':
                # Exit
                break

if __name__ == '__main__':
    # Create menu structure
    menu = RootMenu([
             Menu("Movies", [Product("DVD", 2.50),
                             Product("Blue-Ray", 3.50)]),
             Menu("Games", [Product("New Game", 4.00),
                            Product("Old Game", 2.50)]),
             ])

    # Run menu system
    menu.process(Bill())
