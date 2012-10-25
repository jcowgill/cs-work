# Product Menu

while True:
    # Display top menu
    print "Main Menu"
    print "------------"
    print "1) Movie"
    print "2) Game"
    print "X) Exit"

    # Test selection
    selection = raw_input()
    print

    if selection == '1':
        while True:
            # Movie Menu
            print "Movies"
            print "---------"
            print "1) DVD"
            print "2) Blue Ray"
            print "X) Exit"

            # Test selection
            selection = raw_input()
            print
            
            if selection == '1':
                print "Price = 2.50"
            elif selection == '2':
                print "Price = 3.50"
            elif selection.upper() != 'X':
                continue

            break
        
    elif selection == '2':
        while True:
            # Games Menu
            print "Games"
            print "-------------"
            print "1) New Game"
            print "2) Old Game"
            print "X) Exit"

            # Test selection
            selection = raw_input()
            print
            
            if selection == '1':
                print "Price = 4.00"
            elif selection == '2':
                print "Price = 2.50"
            elif selection.upper() != 'X':
                continue

            break

    elif selection.upper() != 'X':
        continue

    break
    
        
