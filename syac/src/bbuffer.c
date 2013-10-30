#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#include "bbuffer.h"

// Blocking buffer implementation
//

bbuffer * bbuffer_create(size_t capacity)
{
    // Disallow zero sized buffers (which cause infinite blocking)
    assert(capacity > 0);

    // Allocate structure
    bbuffer * buf = malloc(sizeof(bbuffer));
    if (buf == NULL)
        return NULL;

    // Allocate buffer
    buf->data = malloc(capacity);
    if (buf->data == NULL)
    {
        free(buf);
        return NULL;
    }

    // Create locks
    pthread_mutex_init(&buf->lock, NULL);
    pthread_cond_init(&buf->enqueue_cond, NULL);
    pthread_cond_init(&buf->dequeue_cond, NULL);

    // Set initial values
    buf->capacity = capacity;
    buf->start = 0;
    buf->count = 0;
    return buf;
}

void bbuffer_destroy(bbuffer * buf)
{
    // Destroy locks
    pthread_cond_destroy(&buf->dequeue_cond);
    pthread_cond_destroy(&buf->enqueue_cond);
    pthread_mutex_destroy(&buf->lock);

    // Free data
    free(buf->data);
    free(buf);
}

size_t bbuffer_size(bbuffer * buf)
{
    return buf->count;
}

static bool bbuffer_enqueue_impl(bbuffer * buf, char c, bool can_block)
{
    bool result = false;

    // Enter main lock
    pthread_mutex_lock(&buf->lock);

    // Wait for space in the buffer
    while (buf->count >= buf->capacity)
    {
        // Exit if we're not allowed to block
        if (!can_block)
            goto leave_func;

        // Block until there is space
        pthread_cond_wait(&buf->enqueue_cond, &buf->lock);
    }

    // Add data to buffer
    buf->data[(buf->start + buf->count) % buf->capacity] = c;
    buf->count++;

    // Signal any dequeueing threads
    pthread_cond_signal(&buf->dequeue_cond);
    result = true;

leave_func:
    // Leave lock
    pthread_mutex_unlock(&buf->lock);
    return result;
}

void bbuffer_enqueue(bbuffer * buf, char c)
{
    bbuffer_enqueue_impl(buf, c, true);
}

bool bbuffer_try_enqueue(bbuffer * buf, char c)
{
    return bbuffer_enqueue_impl(buf, c, false);
}

static bool bbuffer_dequeue_impl(bbuffer * buf, char * c, bool can_block)
{
    bool result = false;

    // Enter main lock
    pthread_mutex_lock(&buf->lock);

    // Wait for data in the buffer
    while (buf->count == 0)
    {
        // Exit if we're not allowed to block
        if (!can_block)
            goto leave_func;

        // Block until there is data
        pthread_cond_wait(&buf->dequeue_cond, &buf->lock);
    }

    // Extract data from buffer
    *c = buf->data[buf->start];
    buf->start = (buf->start + 1) % buf->capacity;
    buf->count--;

    // Signal any enqueueing threads
    pthread_cond_signal(&buf->enqueue_cond);
    result = true;

leave_func:
    // Leave lock
    pthread_mutex_unlock(&buf->lock);
    return result;
}

char bbuffer_dequeue(bbuffer * buf)
{
    char c;
    bbuffer_dequeue_impl(buf, &c, true);
    return c;
}

bool bbuffer_try_dequeue(bbuffer * buf, char * c)
{
    return bbuffer_dequeue_impl(buf, c, false);
}

void * printer_thread(void * arg)
{
    bbuffer * buf = arg;

    for (;;)
        putchar(bbuffer_dequeue(buf));
}

int main(void)
{
    pthread_t reader;
    bbuffer * buf;

    // Create buffer
    buf = bbuffer_create(1024);
    assert(buf);

    // Create reading thread
    pthread_create(&reader, NULL, printer_thread, buf);

    // Get some characters and put them on the buffer
    int c;

    while ((c = getchar()) != EOF)
        bbuffer_enqueue(buf, c);

    return 0;
}
