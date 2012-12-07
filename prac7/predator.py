# Predator Prey Problem
# James Cowgill
#


def predator_sim(n, r_birth_rate, f_death_rate, r_init, f_init, interaction):
    '''Runs the Predator - Prey simulator using the given initial values

    Returns a list of tuples with (rabbits, foxes)'''

    r_current = r_init
    f_current = f_init
    result = [(r_init, f_init)]

    # Do simulation
    for i in range(1, n):
        # Simulate this turn
        turn_interaction = interaction * r_current * f_current

        r_new = int((r_birth_rate + 1) * r_current - turn_interaction)
        f_new = int((1 - f_death_rate) * f_current + turn_interaction)

        # Do not allow negative numbers
        if r_new < 0:
            r_new = 0
        if f_new < 0:
            f_new = 0

        # Save this turn and update currents
        result.append((r_new, f_new))
        r_current = r_new
        f_current = f_new

    return result
