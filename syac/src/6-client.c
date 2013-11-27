#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <netdb.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

static int print_usage(char * programName)
{
    fprintf(stderr, "Usage:\n  %s host port\n\n", programName);
    fprintf(stderr, "Connects to the given host and allows communication to it via\n");
    fprintf(stderr, "stdin and stdout.\n");
    fprintf(stderr, "\n");
    return 1;
}

// Same as in 6-server.c
static bool parse_port(uint16_t * result, char * input, char * program_name)
{
    // Extract port number
    long long_port = strtol(input, NULL, 0);
    if (long_port <= 0 || long_port > 0xFFFF)
    {
        fprintf(stderr, "%s: port number out of range\n", program_name);
        return false;
    }

    // Store result
    *result = (uint16_t) long_port;
    return true;
}

int main(int argc, char * argv[])
{
    // Check program name
    if (argv[0] == NULL)
        argv[0] = "client";

    // Print usage
    if (argc != 3)
        return print_usage(argv[0]);

    // Extract port number
    uint16_t port;
    if (!parse_port(&port, argv[2], argv[0]))
        return 1;

    // Resolve hostname
    struct hostent * hostname = gethostbyname(argv[1]);
    if (hostname == NULL)
    {
        perror("error resolving hostname");
        return 1;
    }

    // Construct sockaddr_in
    struct sockaddr_in address;
    address.sin_family = AF_INET;
    address.sin_port = htons(port);
    memcpy(&address.sin_addr, hostname->h_addr_list[0], sizeof(address.sin_addr));

    // Create socket + connect to server
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1)
    {
        perror("error creating socket");
        return 1;
    }

    if (connect(sock, (struct sockaddr *) &address, sizeof(address)) == -1)
    {
        perror("error connecting");
        return 1;
    }

    #error TODO This

    return 1;
}
