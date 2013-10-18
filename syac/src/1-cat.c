#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

// Simple cat implementation (no switches allowed)

#define BUF_SIZE 1024

// Copies the contents of the given fd to standard output
//  Returns false on error (code in errno)
//  No files are opened or closed
bool cat_file(int fd)
{
    char buf[BUF_SIZE];
    ssize_t bytes_read;

    // Read into buffer until EOF
    while ((bytes_read = read(fd, buf, BUF_SIZE)) > 0)
    {
        char * bufPtr = buf;

        // Write all bytes to stdout
        do
        {
            ssize_t bytes_written = write(STDOUT_FILENO, bufPtr, bytes_read);

            // Check for errors
            if (bytes_written < 0)
                return false;

            // Update bufPtr and bytes left to process
            bufPtr += bytes_written;
            bytes_read -= bytes_written;

        }
        while (bytes_read > 0);
    }

    // Return read error status
    return bytes_read >= 0;
}

// Processes the list of files given
//  Returns the first file which erred or NULL if OK
//  List of files is NULL terminated
const char * process_files(const char * files[])
{
    // If files is already empty, use stdin
    if (*files == NULL)
    {
        if (!cat_file(STDIN_FILENO))
            return "stdin";
    }
    else
    {
        for (; *files; files++)
        {
            // Handle stdin separately
            if (strcmp(*files, "-") == 0)
            {
                if (!cat_file(STDIN_FILENO))
                    return "stdin";
            }
            else
            {
                // Open file
                int fd = open(*files, O_RDONLY);

                if (fd < 0)
                    return *files;

                // Cat file
                bool cat_result = cat_file(fd);

                // Close file and return on error
                close(fd);
                if (!cat_result)
                    return *files;
            }
        }
    }

    // OK
    return NULL;
}

// Main cat routine
//  Copies given files to stdout
//  The file - indicates standard input
int main(int argc, const char * argv[])
{
    (void) argc;    // Prevent argc unused warning

    // Process files
    const char * err_file = process_files(argv + 1);

    if (err_file != NULL)
    {
        // Print error
        printf("%s: %s: %s\n", argv[0], err_file, strerror(errno));
        return 1;
    }

    return 0;
}
