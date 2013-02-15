# Stacker Mini Assignment
# James Cowgill
#


def move(max_pop, stack, me=0):
    # If we can, take all the items left
    if len(stack) <= max_pop:
        if me == 0:
            return (sum(stack), 0)
        else:
            return (0, sum(stack))

    # Try each number of items
    sub_stack = stack[:]
    my_sum = 0
    best = (0, 0)

    for _ in range(max_pop):
        # Pop last item and add it to the sum
        my_sum += sub_stack.pop()

        # Try this
        new_left, new_right = move(max_pop, sub_stack, 1 - me)

        # Handle player differences
        if me == 0:
            # Add sum
            new_left += my_sum

            if new_left > best[0]:
                best = (new_left, new_right)
        else:
            # Add sum
            new_right += my_sum

            if new_right > best[1]:
                best = (new_left, new_right)

    # Return the best situation
    return best

if __name__ == '__main__':
    # Get inputs from use
    first_line = raw_input().split()
    lines = int(first_line[0])
    max_pop = int(first_line[1])

    # Get input stack
    stack = []
    for i in range(lines):
        stack.append(int(raw_input()))

    # Do calculations
    best = move(max_pop, stack)

    # Print result
    print best[0]
