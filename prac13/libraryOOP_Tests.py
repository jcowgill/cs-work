##import libraryOOP
from libraryOOP import *

my_library = Library()

print my_library


print 'Adding member :', my_library.add_member(Member('lilian','blot','xxx', 'm0001'))
print 'Adding member :', my_library.add_member(Member('will','Smith','xxx', 'm0002'))
print 'Adding member :', my_library.add_member(Member('Alan','Sharpe','xxx', 'm0002')) ## should not be added, existing uid

print my_library

print 'Adding item :', my_library.add_item(DVD('Le grand bleu','J.L. Besson','U', 'dvd_0007'))
print 'Adding item :', my_library.add_item(DVD('Le grand bleu','J.L. Besson','U', 'dvd_0008'))
print 'Adding item :', my_library.add_item(DVD('Fawlty Towers','J. Cleese','PG', 'dvd_0008')) ## should not be added, existing uid
print 'Adding item :', my_library.add_item(DVD('Fawlty Towers','J. Cleese','PG', 'dvd_0010')) ## should be added, not an existing uid

print my_library

print 'Borrowing Item:', my_library.borrow('dvd_0007', 'm0001')

print my_library

print 'Borrowing Item:', my_library.borrow('dvd_0007', 'm0002') ## item already borrowed so must return false

print my_library

print 'Borrowing Item:', my_library.borrow('dvd_0008', 'm0001')
print 'Borrowing Item:', my_library.borrow('dvd_0010', 'm0002')

print my_library

print 'returning Item:', my_library.return_item('dvd_0008')

print my_library

print 'returning Item:', my_library.return_item('dvd_0008') ## item already returned so must return false
print 'returning Item:', my_library.return_item('dvd_0010') ## item does not exist so must return false

print my_library

print 'delete item:', my_library.delete_item('dvd_0008')
print 'delete item:', my_library.delete_item('dvd_0007') ## item not returned yet so must return false
print 'delete item:', my_library.delete_item('dvd_0010') ## item does not exist so must return false

print my_library

print 'delete member:', my_library.delete_member('m0002')
print 'delete member:', my_library.delete_member('m0001') ## item not returned yet so must return false
print 'delete member:', my_library.delete_member('m0009') ## member does not exist so must return false

print my_library
