#include <fcntl.h>
#include <stdio.h>

int main(void)
{
    int fd = open("/tmp/out2.txt", O_WRONLY | O_CREAT | O_TRUNC, 0644);

    if (fd < 0)
    {
        perror("1-open2");
        return 1;
    }

    printf("%d\n", fd);
    return 0;
}
