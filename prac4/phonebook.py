# Phonebook
#

phonebook = {'zarniwoop':{'home':'1234567890', 'mobile':'0909090909'},
             'emmet':{'home':'78979879', 'fax':'6546'}}

# Show the names
print "Phonebook"
print "================"
for name, numbers in phonebook.items():
    print
    print name
    print "---"
    for num_type, number in numbers.items():
        print format(num_type, '10'), ":", number
        
