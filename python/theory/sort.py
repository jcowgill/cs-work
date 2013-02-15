# Merge Sort, Insertion Sort Hybrid
#  James Cowgill
#


def insertion_sort(lst):
    '''Performs an in-place insertion sort on the given list'''
    for i in range(1, len(lst)):
        # Get the current value
        value = lst[i]

        # Go backwards shuffling elements to make room
        shuffle_pos = i - 1
        while shuffle_pos >= 0 and lst[shuffle_pos] > value:
            lst[shuffle_pos + 1] = lst[shuffle_pos]
            shuffle_pos -= 1

        # Insert our new element
        lst[shuffle_pos + 1] = value

    return lst


def merge_sort(lst, insert_sort_limit=1):
    '''Performs a merge sort on the given list and returns the new list

    When sorting less than insert_sort_limit items, will defer to using
    insertion_sort'''

    def merge(left, right):
        '''Merge 2 lists to create another one'''

        new_list = []
        left_ptr = 0
        right_ptr = 0

        while left_ptr < len(left) and right_ptr < len(right):
            # Add the smaller one (or left if the same)
            if right[right_ptr] < left[left_ptr]:
                new_list.append(right[right_ptr])
                right_ptr += 1

            else:
                new_list.append(left[left_ptr])
                left_ptr += 1

        # Add whatever is left
        while left_ptr < len(left):
            new_list.append(left[left_ptr])
            left_ptr += 1

        while right_ptr < len(right):
            new_list.append(right[right_ptr])
            right_ptr += 1

        # Done
        return new_list

    def do_sort(start, end):
        '''Does a merge sort on lst with the given start and end'''
        if end - start <= insert_sort_limit:
            # Use insertion sort now
            return insertion_sort(lst[start:end])

        else:
            # Do recursive calls
            left = do_sort(start, (start + end) // 2)
            right = do_sort((start + end) // 2, end)

            # Merge the lists
            return merge(left, right)

    # Validate insertion sort limit
    if insert_sort_limit < 1:
        raise ValueError("insert_sort_limit must be >= 1")

    return do_sort(0, len(lst))


if __name__ == '__main__':
    import timeit

    def do_test(stmt):
        setup = '''
import random
from __main__ import insertion_sort, merge_sort
A = [int(10000 * random.random()) for i in xrange(1000)]
'''
        return min(timeit.repeat(stmt, setup, repeat=100, number=1))

    # Do the timimg
    print "Normal Insertion", do_test("insertion_sort(A)")
    print "Normal Merge    ", do_test("merge_sort(A)")
    print "Hybrid Merge Ins", do_test("merge_sort(A, 30)")
