#include <inttypes.h>
#include <stddef.h>

#include "5-buddy.h"

// Minimum block size (must be power of 2)
#define MIN_SIZE 16

// Maximum block order
#define ORDER_COUNT 16

// Amount of total data to allocatelin
#define DATA_SIZE (MIN_SIZE * (1 << ORDER_COUNT))

// Information about one order of the allocator
//  The buddy map stores 2 bits per block
//   00 = Free block / block is part of a larger block
//   01 = Right block allocated
//   10 = Left block allocated
//   11 = Both blocks allocated SEPARATELY (ie to different alloc calls)
typedef struct buddy_order
{
    void ** free_list;  // Linked list of free blocks
    uint8_t * map;      // Buddy map (null for smallest order)

} buddy_order;

// Data for the allocator
static uint8_t buddy_data[DATA_SIZE] __attribute__((aligned(MIN_SIZE)));
static uint8_t buddy_map_data[(1 << ORDER_COUNT) / 4];

// Order data
static buddy_order buddy_orders[ORDER_COUNT];

// Fast calculation of ceil(log_2(val))
static uint8_t clog2_int(uint32_t val);

void buddy_init()
{
    uint8_t * map_data_pos = buddy_map_data;
    uint32_t order_size = sizeof(buddy_map_data) / 2;

    // Initialize all buddy orders (except 0 which is left NULL)
    for (int i = 1; i < ORDER_COUNT; i++)
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
    // Calculate lowest order containing required number of bytes
    if (bytes == 0)
        return NULL;

    bytes = clog2_int(bytes);

    return NULL;

    // Search the free lists
}

void buddy_free(void * ptr)
{
    // Ignore null ptrs
    if (ptr == NULL)
        return;
}

static uint8_t clog2_int(uint32_t val)
{
    // Log lookup table
    static const uint8_t log_table[256] =
    {
#define LT(n) n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n
        -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        LT(4), LT(5), LT(5), LT(6), LT(6), LT(6), LT(6),
        LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7)
#undef LT
    };

    uint32_t high_bits = val >> 16;
    uint32_t middle_bits;

    // Split val down to 8 bit chunks which we pass to the lookup table
    if (high_bits != 0)
    {
        middle_bits = high_bits >> 8;
        if (middle_bits != 0)
            return 24 + log_table[middle_bits];
        else
            return 16 + log_table[high_bits];
    }
    else
    {
        middle_bits = val >> 8;
        if (middle_bits != 0)
            return 8 + log_table[middle_bits];
        else
            return log_table[val];
    }
}

#if 1

#include <stdio.h>
#include <math.h>

int main(void)
{
    // Test clog2_int
    for (uint32_t i = 1; i < 100; i++)
    {
        printf("clog2_int(%u) = %u\n", i, clog2_int(i));
 //       uint32_t res = clog2_int(i);
 //       double act = ceil(log2(i));
 //       if ((double) res - act > 0.001)
 //           printf("Fail clog2_int = %u, ceil(log2) = %f\n", res, act);
    }

    //buddy_init();
    return 0;
}

#endif
