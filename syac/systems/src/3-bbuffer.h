#ifndef _BBUFFER_H
#define _BBUFFER_H

// Thread safe fixed size blocking buffer implementation
//

#include <stdbool.h>
#include <stddef.h>
#include <pthread.h>

/// The main buffer structure
typedef struct bbuffer
{
    char * data;            ///< Array containing the buffer's data
    size_t capacity;        ///< Buffer maximum capacity

	size_t start;           ///< Index of first valid character
	size_t count;           ///< Number of valid characters (wraps around)

	pthread_mutex_t lock;   ///< Main buffer lock
	pthread_cond_t enqueue_cond;    ///< Condition variable waited on during enqueue
	pthread_cond_t dequeue_cond;    ///< Condition variable waited on during dequeue

} bbuffer;

/// Creates a new buffer
/// @param capacity maximum size of the buffer
/// @return the buffer or NULL if no memory is available
bbuffer * bbuffer_create(size_t capacity);

/// Destroys a buffer
/// @param buf buffer to destroy
void bbuffer_destroy(bbuffer * buf);

/// Returns the current size of the buffer (number of elements)
/// @param buf buffer to get size of
/// @return the current number of elements in the buffer
size_t bbuffer_size(bbuffer * buf);

/// Enqueues a character (blocking if there is no space)
/// @param buf buffer to enqueue to
/// @param c   character to enqueue
void bbuffer_enqueue(bbuffer * buf, char c);

/// Tries to enqueue a character
/// @param buf buffer to enqueue to
/// @param c   character to enqueue
/// @return <b>true</b> if the character was enqueued, <b>false</b> if not
bool bbuffer_try_enqueue(bbuffer * buf, char c);

/// Dequeues a character (blocking if there are none)
/// @param buf buffer to dequeue from
/// @return the dequeued character
char bbuffer_dequeue(bbuffer * buf);

/// Tries to dequeue a character
/// @param buf buffer to dequeue from
/// @param c   pointer to location to store the character
/// @return <b>true</b> if the character was dequeued, <b>false</b> if not
bool bbuffer_try_dequeue(bbuffer * buf, char * c);

#endif
