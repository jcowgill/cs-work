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

#define BASE_SIZE (sizeof(block_list_data))
#define TOTAL_BLOCKS (1 << ORDER_COUNT)

// Data for the allocator
static block_list_data buddy_data[TOTAL_BLOCKS];
static uint8_t buddy_map_data[TOTAL_BLOCKS];

// Order data
static buddy_order buddy_orders[ORDER_COUNT];

// Calculates the order from the size requested
static uint32_t order_from_size(size_t bytes);

// Get a pointer to the bitmap element for an order and address
static uint8_t * bitmap_ptr(block_list_data * ptr, uint32_t order);

// Inserts a block at the beginning of the given free list
static void free_list_insert(block_list_data * block, uint32_t order);

// Removes a block from the free list
static void free_list_remove(block_list_data * block, uint32_t order);

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
    buddy_orders[ORDER_COUNT - 1].free_list = buddy_data;
}

void * buddy_allocate(size_t bytes)
{
    // Ignore size 0 allocations
    if (bytes == 0)
        return NULL;

    // Calculate lowest order containing required number of bytes
    uint32_t req_order = order_from_size(bytes);

    // Check each order (>= requested order) for free blocks
    for (uint32_t order = req_order; order < ORDER_COUNT; order++)
    {
        block_list_data * block = buddy_orders[order].free_list;

        if (block != NULL)
        {
            // Update free list
            if (block->next != NULL)
                block->next->prev = NULL;

            buddy_orders[order].free_list = block->next;

            // Split blocks if this one is too big
            for (; order > req_order; order--)
            {
                // Mark this block as allocated
                *bitmap_ptr(block, order) = 1;

                // Insert right half of block onto next order's list
                free_list_insert(block + (1 << (order - 1)), order - 1);
            }

            // Set block as allocated in my order
            *bitmap_ptr(block, req_order) = 1;
            return block;
        }
    }

    // Not enough memory :(
    return NULL;
}

void buddy_free(void * ptr)
{
    block_list_data * my_block = ptr;

    // Ignore null ptrs
    if (my_block == NULL)
        return;

    // Find this ptr's order
    uint32_t order;
    for (order = 0; order < ORDER_COUNT; order++)
    {
        if (*bitmap_ptr(my_block, order) == 1)
            break;
    }

    assert (order < ORDER_COUNT);

    // Coalesce blocks
    for (; order < (ORDER_COUNT - 1); order++)
    {
        // Calculate all the pointers to the blocks and bitmaps
        uint8_t * my_bitmap = bitmap_ptr(my_block, order);
        int other_offset = ((uintptr_t) my_bitmap & 1) ? -1 : 1;

        uint8_t * other_bitmap = my_bitmap + other_offset;
        block_list_data * other_block = my_block + (other_offset << order);

        // Exit if block cannot be coalesced
        if (*other_bitmap != 0)
            break;

        // Free my block, remove other from free list and adjust final pointer
        *my_bitmap = 0;
        free_list_remove(other_block, order);

        if (other_block < my_block)
            my_block = other_block;
    }

    // Free the last block and add to free list
    *bitmap_ptr(my_block, order) = 0;
    free_list_insert(my_block, order);
}

static uint32_t order_from_size(size_t bytes)
{
    // Calculate number of blocks required
    size_t blocks = (bytes + BASE_SIZE - 1) / BASE_SIZE;
    assert(blocks != 0);

    // Do ceil(log2) to get the order
    uint32_t order = (uint32_t) ceil(log2(blocks));
    assert(BASE_SIZE * (1 << order) >= bytes);
    return order;
}

static uint8_t * bitmap_ptr(block_list_data * ptr, uint32_t order)
{
    // Verify ptr is in range
    assert(ptr >= buddy_data && ptr < (buddy_data + TOTAL_BLOCKS));

    // Get and validate block number
    ptrdiff_t block_no = (ptr - buddy_data) >> order;
    assert(buddy_data + (block_no << order) == ptr);

    return buddy_orders[order].map + block_no;
}

static void free_list_insert(block_list_data * block, uint32_t order)
{
    block->prev = NULL;
    block->next = buddy_orders[order].free_list;
    buddy_orders[order].free_list = block;
}

static void free_list_remove(block_list_data * block, uint32_t order)
{
    if (block->prev == NULL)
        buddy_orders[order].free_list = block->next;
    else
        block->prev->next = block->next;

    if (block->next != NULL)
        block->next->prev = block->prev;
}

#if 1

int main(void)
{
    buddy_init();

    // Allocate and free some stuff
    void * ptr = buddy_allocate(42);
    void * ptr2 = buddy_allocate(1000);
    void * ptr3 = buddy_allocate(1);

    buddy_free(ptr);
    buddy_free(ptr2);
    buddy_free(ptr3);

    return 0;
}

#endif
