#include <stddef.h>

// Returns the length of the given string
size_t strlen(const char * str)
{
    size_t len = 0;

    while (*str++)
        len++;

    return len;
}

// Copies src into dest (along with the NULL char)
char * strcpy(char * dest, const char * src)
{
    char * destStart = dest;

    while (*src)
        *dest++ = *src++;

    *dest = '\0';
    return destStart;
}

// Appends src to dest
char * strcat(char * dest, const char * src)
{
    // Go to end of dest string
    char * newDest = (dest + strlen(dest));

    // Copy src into newDest
    strcpy(newDest, src);

    return dest;
}

int main(void)
{
}
