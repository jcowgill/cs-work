#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// Simple shell

#define COMMAND_DEFSIZE      8
#define EXPAND_STR_DEFSIZE  16

// Contents of argv extracted from the command line
typedef struct command
{
    char ** argv;

    unsigned count;
    unsigned capacity;

} command;

// Expandable string
typedef struct expand_str
{
    char * str;

    unsigned count;
    unsigned capacity;

} expand_str;

// Append a character to an expand_str
bool expand_str_append(expand_str * str, char c)
{
    if (str->str == NULL)
    {
        // Allocate new string
        str->str = malloc(EXPAND_STR_DEFSIZE * sizeof(char));

        if (str->str == NULL)
            return false;

        str->count = 0;
        str->capacity = EXPAND_STR_DEFSIZE;
    }
    else if (str->count >= str->capacity)
    {
        // Expand string
        char * new_str = realloc(str->str, str->capacity * 2);

        if (new_str == NULL)
            return false;

        str->str = new_str;
        str->capacity *= 2;
    }

    // Append character
    str->str[str->count++] = c;
    return true;
}

// Append an argument to a command
bool command_append(command * cmd, char * arg)
{
    if (cmd->argv == NULL)
    {
        // Allocate new args list
        cmd->argv = malloc(COMMAND_DEFSIZE * sizeof(char *));

        if (cmd->argv == NULL)
            return false;

        cmd->count = 0;
        cmd->capacity = COMMAND_DEFSIZE;
    }
    else if (cmd->count >= cmd->capacity)
    {
        // Expand string
        char ** new_argv = realloc(cmd->argv, cmd->capacity * 2);

        if (new_argv == NULL)
            return false;

        cmd->argv = new_argv;
        cmd->capacity *= 2;
    }

    // Append arg
    cmd->argv[cmd->count++] = arg;
    return true;
}

// Frees the DATA and ALL STRINGS in a command
void command_free(command * cmd)
{
    if (cmd->argv != NULL)
    {
        for (unsigned i = 0; i < cmd->count; i++)
            free(cmd->argv[i]);

        free(cmd->argv);
        cmd->argv = NULL;
    }
}

// Parses a command from the given input stream
//  Arguments are appended to the output command
//  Returns false on error or eof
bool parse_command(FILE * input, command * output)
{
    int c;
    bool last_was_white = true;
    expand_str current_str = { NULL, 0, 0 };

    // Read space separated strings until a newline
    while ((c = fgetc(input)) != EOF)
    {
        // Handle whitespace
        if (isspace(c))
        {
            if (!last_was_white)
            {
                // Complete argument and append to list
                if (!expand_str_append(&current_str, '\0'))
                    goto err_handler;

                if (!command_append(output, current_str.str))
                    goto err_handler;

                // Reset current string
                current_str.str = NULL;
            }

            // Finish command sucessfully
            if (c == '\n')
                return command_append(output, NULL);

            last_was_white = true;
        }
        else
        {
            // Append character
            expand_str_append(&current_str, c);
            last_was_white = false;
        }
    }

    // If an eof occured, ensure errno is 0
    if (feof(input) && !ferror(input))
        errno = 0;

err_handler:
    // Error or eof occured
    free(current_str.str);
    return false;
}

// Sets handler for shell related signals
void set_job_signals(void (* mode)(int))
{
    signal(SIGINT,  mode);
    signal(SIGQUIT, mode);
    signal(SIGTSTP, mode);
    signal(SIGTTIN, mode);
    signal(SIGTTOU, mode);
    signal(SIGCHLD, mode);
}

int main(int argc, char * argv[])
{
    bool is_parent = true;
    command cmd = { NULL, 0, 0 };

    (void) argc;

    // This program only works on interactive shells
    if (!isatty(STDIN_FILENO))
    {
        fprintf(stderr, "%s: %s\n", argv[0], strerror(ENOTTY));
        return EXIT_FAILURE;
    }

    // Disable job signals
    set_job_signals(SIG_IGN);

    // Enter main loop
    for (;; command_free(&cmd))
    {
        // Print prompt
        fprintf(stderr, "$ ");

        // Get command
        if (!parse_command(stdin, &cmd))
            break;

        // Check for exit command
        if (cmd.count == 2 && strcmp(cmd.argv[0], "exit") == 0)
        {
            errno = 0;
            break;
        }

        // Ignore empty commands
        if (cmd.count >= 2)
        {
            // Fork process
            pid_t pid = fork();

            if (pid == 0)
            {
                // Setup new process group
                setpgid(0, 0);

                // Make us the foreground process group
                tcsetpgrp(STDIN_FILENO, getpgrp());

                // Reset job signals
                set_job_signals(SIG_DFL);

                // Execute command
                execvp(cmd.argv[0], cmd.argv);

                // Report error and exit
                is_parent = false;
                break;
            }
            else
            {
                // Wait for child process group to exit
                while (wait(NULL) != -1)
                    ;   // Keep waiting...

                // Put shell back as foreground process group
                tcsetpgrp(STDIN_FILENO, getpgrp());
            }
        }
    }

    // Handle errors / eof
    int status;

    if (errno != 0)
    {
        status = EXIT_FAILURE;

        if (is_parent)
            fprintf(stderr, "%s (shell error): %s\n", argv[0], strerror(errno));
        else
            fprintf(stderr, "%s: %s: %s\n", argv[0], cmd.argv[0], strerror(errno));
    }
    else
    {
        status = EXIT_SUCCESS;
        fprintf(stderr, "\n");
    }

    command_free(&cmd);
    return status;
}
