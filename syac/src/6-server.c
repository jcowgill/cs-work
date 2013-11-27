#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

#define LISTENER_BACKLOG 16

static int print_usage(char * programName)
{
    fprintf(stderr, "Usage:\n  %s port command...\n\n", programName);
    fprintf(stderr, "Listens for TCP connections on the given port and runs\n");
    fprintf(stderr, "a program for each connection received.\n");
    fprintf(stderr, "stdin and stdout are mapped to the connection.\n");
    fprintf(stderr, "\n");
    return 1;
}

static int create_listener(uint16_t port)
{
    // Create socket
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1)
        return -1;

    // Bind to port
    struct sockaddr_in address;

    address.sin_family = AF_INET;
    address.sin_port = htons(port);
    address.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(sock, (struct sockaddr *) &address, sizeof(address)) == -1)
        goto err_handler;

    // Start listening
    if (listen(sock, LISTENER_BACKLOG) == -1)
        goto err_handler;

    return sock;

err_handler:
    close(sock);
    return -1;
}

static bool should_retry_accept(int error)
{
    // Retry accept if it returns any of these errors
    //  See accept(2)
    return  error == EAGAIN ||
            error == EHOSTDOWN ||
            error == EHOSTUNREACH ||
            error == ENETDOWN ||
            error == ENETUNREACH ||
            error == ENONET ||
            error == ENOPROTOOPT ||
            error == EOPNOTSUPP ||
            error == EPROTO;
}

void setenv_number(char * name, uint16_t num)
{
    char value[6];  // 6 = ceil(log10(USHORT_MAX)) + 1
    char * value_ptr = &value[sizeof(value) - 1];

    // Convert num to a string (does not work with 0)
    *value_ptr = '\0';

    while (num > 0)
    {
        *--value_ptr = num % 10;
        num /= 10;
    }

    // Set variable
    setenv(name, value_ptr, 1);
}

int main(int argc, char * argv[])
{
    // Check program name
    if (argv[0] == NULL)
        argv[0] = "server";

    // Print usage
    if (argc < 3)
        return print_usage(argv[0]);

    // Extract port number
    long long_port = strtol(argv[1], NULL, 0);
    if (long_port <= 0 || long_port > 0xFFFF)
    {
        fprintf(stderr, "%s: port number out of range\n", argv[0]);
        return 1;
    }

    // Setup constant parts of environment
    uint16_t port = (uint16_t) long_port;
    setenv("PROTO", "TCP", 1);
    setenv("TCPLOCALIP", "0.0.0.0", 1);
    setenv_number("TCPLOCALPORT", port);

    // Start listening socket
    int listener = create_listener(port);
    if (listener == -1)
    {
        perror("error creating listener");
        return 1;
    }

    // Process incoming connections
    for (;;)
    {
        // Accept new connection
        struct sockaddr_in child_addr;
        socklen_t child_addr_len = sizeof(child_addr);
        int child_sock = accept(listener, (struct sockaddr *) &child_addr, &child_addr_len);

        if (child_sock != -1)
        {
            // Create child process
            pid_t child_pid = fork();
            if (child_pid == -1)
            {
                perror("error forking child process");
                return 1;
            }

            if (child_pid == 0)
            {
                // Copy child socket onto stdin, stdout and stderr
                dup2(child_sock, STDIN_FILENO);
                dup2(child_sock, STDOUT_FILENO);
                dup2(child_sock, STDERR_FILENO);

                // Close old sockets
                close(child_sock);
                close(listener);

                // Setup environment
                setenv("TCPREMOTEIP", inet_ntoa(child_addr.sin_addr), 1);
                setenv_number("TCPREMOTEPORT", ntohs(child_addr.sin_port));

                // Run program
                execvp(argv[2], &argv[2]);

                // Report error
                perror("error executing child process");
                return 1;
            }

            // Close child socket now we've passed it on
            close(child_sock);

            // Cleanup any zombie children
            while (waitpid(-1, NULL, WNOHANG) > 0)
                ;
        }
        else
        {
            // Ignore error or exit
            if (!should_retry_accept(errno))
            {
                perror("error accepting connections");
                return 1;
            }
        }
    }
}
