#include <fcntl.h>
#include <stdio.h>

int main(void)
{
    int fd = open("/tmp/in1.txt", O_RDONLY);

    printf("%d\n", fd);
    return 0;
}
