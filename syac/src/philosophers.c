#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

// Dining philosophers problem simulation
//

#define NUM_PHILOSOPHERS    5

static pthread_mutex_t forks[NUM_PHILOSOPHERS];

// Generates the next lfsr number from the given state
static void lfsr_next(uint32_t * state)
{
    // Generate next bit
    uint32_t state_value = *state;
    uint32_t bit = ((state_value >> 0) ^ (state_value >> 10) ^ (state_value >> 30) ^ (state_value >> 31)) & 1;

    *state = (state_value >> 1) | (bit << 31);
}

void * philosopher_thread(void * ptr_id)
{
    int id = (int) ptr_id;
    unsigned int rand_state = time(NULL) + id;
    lfsr_next(&rand_state);

    // Select forks
    int forkA, forkB;

    if (id == NUM_PHILOSOPHERS - 1)
    {
        forkA = 0;
        forkB = id;
    }
    else
    {
        forkA = id;
        forkB = id + 1;
    }

    for (;;)
    {
        // Begin thinking for random time
        printf("%d: Thinking\n", id);
        lfsr_next(&rand_state);
        sleep(1 + (rand_state % 5));
        printf("%d: Finished Thinking\n", id);

        // Pick up forks
        pthread_mutex_lock(&forks[forkA]);
        pthread_mutex_lock(&forks[forkB]);

        // Eat
        printf("%d: Eating\n", id);
        lfsr_next(&rand_state);
        sleep(1 + (rand_state % 5));

        // Put forks down
        pthread_mutex_unlock(&forks[forkA]);
        pthread_mutex_unlock(&forks[forkB]);
    }
}

int main(void)
{
    pthread_t thr;
    printf("Dining Philosphers (%d philosophers)\n\n", NUM_PHILOSOPHERS);

    // Create forks
    for (int i = 0; i < NUM_PHILOSOPHERS; i++)
        pthread_mutex_init(&forks[i], NULL);

    // Create philosophers
    for (int i = 0; i < NUM_PHILOSOPHERS; i++)
        pthread_create(&thr, NULL, philosopher_thread, (void *) i);

    // Hang until SIGINTed
    for (;;)
        pause();
}
