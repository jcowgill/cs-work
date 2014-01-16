#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
    // Open the file lots of times...
    int fd1 = open("/tmp/in1.txt", O_RDONLY);
    int fd2 = open("/tmp/in1.txt", O_RDONLY);
    int fd3 = open("/tmp/in1.txt", O_RDONLY);
    int fd4 = open("/tmp/in1.txt", O_RDONLY);
    int fd5 = open("/tmp/in1.txt", O_RDONLY);

    // Print the descriptors
    printf("Descriptors: %d %d %d %d %d\n", fd1, fd2, fd3, fd4, fd5);

    // Reopen stuff
    close(fd3);
    close(fd2);

    fd2 = open("/tmp/in1.txt", O_RDONLY);
    fd3 = open("/tmp/in1.txt", O_RDONLY);

    printf("Reopened fd2 and fd3\n");
    printf("Descriptors: %d %d %d %d %d\n", fd1, fd2, fd3, fd4, fd5);

    return 0;
}
