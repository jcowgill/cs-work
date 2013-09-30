#include <stdio.h>

int main(void)
{
    int hours;
    double rate;

    // Read inputs
    puts("How many (whole) hours has the employee worked?");
    scanf("%d", &hours);
    getchar();

    puts("What is the hourly rate?");
    scanf("%lf", &rate);
    getchar();

    // Print result
    printf("Total earnings $%.2f\n", rate * hours);
    return 0;
}
