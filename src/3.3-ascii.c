#include <stdio.h>

#define CHARS_PER_LINE 8

int main(void)
{
    // Print the table of ASCII characters
    for (int i = 0; i < 256; i++)
    {
        char c = i;

        // Convert special characters to 0xFF so they dont kill the table
        if (c == '\n' || c == '\t' || c == '\b' || c == '\0' ||
            c == '\v' || c == '\f' || c == '\r')
        {
            c = '\xFF';
        }

        // Print this character
        printf("%2x %c  ", i, c);

        // Print new line if needed
        if ((i + 1) % CHARS_PER_LINE == 0)
            printf("\n");
    }

    // Print final new line and return
    printf("\n");
    return 0;
}
