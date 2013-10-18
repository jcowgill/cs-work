#include <stdio.h>

// Converts lowercase characters to uppercase
char to_upper(char c)
{
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 'A';
    else
        return c;
}

int main(void)
{
    // Convert all input chars to upper case
    int c;

    while ((c = getchar()) != EOF)
        putchar(to_upper(c));

    return 0;
}
