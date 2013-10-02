// This is a replacement for the 2.1-birthdays.c file
//  which is (hopefully) more secure and better :)

#include <stdio.h>

// Table of day of week offsets
static int month_table[] = {0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4};

// Table of day of week strings
static const char * doy_strings[] =
{
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
};

// Calculates the day of the week for the given arguments
int calculate_dow(unsigned year, unsigned month, unsigned day)
{
    // Michael Keith and Tom Craver. (1990). The ultimate perpetual calendar?,
    //  Journal of Recreational Mathematics, 22:4, pp.280-282

    if (month < 3)
        year--;

    unsigned yearOffset = year + year / 4 - year / 100 + year / 400;
    unsigned monthOffset = month_table[month - 1];

    return (yearOffset + monthOffset + day) % 7;
}

int main(void)
{
    char buffer[256];
    int exitCode = 1;

    // Print header
    fputs("Enter lines of the form 'dd mm yyyy' on each line\n", stderr);
    fputs(" The day of the week will be printed out\n", stderr);
    fputs(" Using the Gregorian calendar only\n", stderr);
    fputs("  (so dates pre-1752 will be inaccurate)\n\n", stderr);

    // Read one line at a time containing the dates
    while (fgets(buffer, sizeof(buffer), stdin) != NULL)
    {
        // Read numbers
        unsigned year, month, day;

        if (sscanf(buffer, "%u %u %u", &day, &month, &year) != 3)
        {
            fputs("Invalid date format\n", stderr);
            continue;
        }

        // Validate ranges
        if (month < 1 || month > 12)
        {
            fputs("Invalid month\n", stderr);
            continue;
        }

        if (day < 1 || day > 31)
        {
            fputs("Invalid day\n", stderr);
            continue;
        }

        // Calculate day of the week and print result
        fprintf(stdout, "%s\n", doy_strings[calculate_dow(year, month, day)]);
        exitCode = 0;
    }

    return exitCode;
}
