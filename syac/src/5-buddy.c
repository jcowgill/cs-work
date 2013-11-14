#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stddef.h>

#include "5-buddy.h"

// Number of orders to use
//  Total Memory = (32 * 2^ORDER_COUNT)
#define ORDER_COUNT 15

// Data stored by a free block (always 32 bytes)
typedef struct block_list_data
{
    struct block_list_data * prev;
    struct block_list_data * next;

    char _pad[32 - (sizeof(void *) * 2)];

} block_list_data;

// Information about one order of the allocator
//  The buddy map stores a 0 or 1
//   0 = Free block / block is part of a larger block
//   1 = Allocated (possibly not all at once though)
typedef struct buddy_order
{
    block_list_data * free_list;    // Linked list of free blocks
    uint8_t * map;                  // Buddy map

} buddy_order;

#define DATA_SIZE (sizeof(block_list_data) * (1 << ORDER_COUNT))

// Data for the allocator
static uint8_t buddy_data[DATA_SIZE] __attribute__((aligned(MIN_SIZE)));
static uint8_t buddy_map_data[1 << ORDER_COUNT];

// Order data
static buddy_order buddy_orders[ORDER_COUNT];

// Calculates the order from the size requested
static uint32_t order_from_size(size_t bytes);

// Get a pointer to the bitmap element for an order and address
static uint8_t * bitmap_ptr(void * ptr, uint32_t order);

void buddy_init()
{
    uint8_t * map_data_pos = buddy_map_data;
    uint32_t order_size = sizeof(buddy_map_data) / 2;

    // Initialize all buddy orders (except 0 which is left NULL)
    for (int i = 0; i < ORDER_COUNT; i++)
    {
        buddy_orders[i].map = map_data_pos;

        map_data_pos += order_size;
        order_size >>= 1;
    }

    // Store main block in free list of largest order
    buddy_orders[ORDER_COUNT - 1].free_list = (void **) buddy_data;
}

void * buddy_allocate(size_t bytes)
{
    // Ignore size 0 allocations
    if (bytes == 0)
        return NULL;

    // Calculate lowest order containing required number of bytes
    uint32_t req_order = order_from_size(bytes);
    assert((1 << req_order) >= bytes);

    // Check each order (>= requested order) for free blocks
    for (uint32_t order = req_order; order < ORDER_COUNT; order++)
    {
        block_list_data * block = buddy_orders[order].free_list;

        if (free_ptr != NULL)
        {
            // Update free list
            if (block->next != NULL)
                block->next->prev = NULL;

            buddy_orders[order].free_list = block->next;

            // Split blocks if this one is too big
            for (; order > req_order; order--)
            {
                // Insert right half of block onto the order's free list
                block_list_data * right = block + (1 << order);
                right->prev = NULL;
                right->next = buddy_orders[order].free_list;
                buddy_orders[order].free_list = right;

                // Mark left as allocated
                *bitmap_ptr(block, order) = 1;
            }

            // Set block as allocated in my order
            *bitmap_ptr(block, req_order) = 1;
            return free_ptr;
        }
    }

    // Not enough memory :(
    return NULL;
}

void buddy_free(void * ptr)
{
    // Ignore null ptrs
    if (ptr == NULL)
        return;

    // Search bitmaps to determine this ptr's order
}

static uint32_t order_from_size(size_t bytes)
{
    if (bytes <= MIN_SIZE)
        return 0;
    else
        return (uint32_t) ceil(log2(bytes / MIN_SIZE));
}

static uint8_t * bitmap_ptr(void * ptr, uint32_t order)
{
    // Verify ptr is in range
    assert(ptr >= buddy_data && ptr < (buddy_data + DATA_SIZE));

    ptrdiff_t block_no = ((ptr - buddy_data) / MIN_SIZE) >> order;
    return buddy_orders[order].map + block_no;
}

#if 1

int main(void)
{
    buddy_init();
    return 0;
}

#endif
