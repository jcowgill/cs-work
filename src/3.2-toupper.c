#include <stdio.h>

// Converts lowercase characters to uppercase
char toupper(char c)
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
        putchar(toupper(c));

    return 0;
}
