#ifndef _BUDDY_H
#define _BUDDY_H

// Buddy memory allocator

#include <stddef.h>

// Initializes the buddy allocator
void buddy_init();

// Allocates at least the given number of bytes
void * buddy_allocate(size_t bytes);

// Frees a pointer allocated with buddy_allocate
void buddy_free(void * ptr);

#endif
