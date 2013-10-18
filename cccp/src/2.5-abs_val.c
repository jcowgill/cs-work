#include <limits.h>
#include <stdio.h>

// Returns the absolute value of val
unsigned int abs_val(int val)
{
    if (val < 0)
        return -val;
    else
        return val;
}

#define ABS_TEST(val) printf("Abs %d -> %u\n", val, abs_val(val))

int main(void)
{
    // Tests
    ABS_TEST(0);
    ABS_TEST(1);
    ABS_TEST(-1);
    ABS_TEST(42);
    ABS_TEST(-10000);
    ABS_TEST(INT_MAX);
    ABS_TEST(INT_MIN);
    return 0;
}
