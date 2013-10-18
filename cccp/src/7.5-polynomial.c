#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct node
{
    int power;
    float coeff;
    struct node * next;

} node;

// Inserts a node at the end of a list
//  previousPtr points to a pointer containing the previous node in the
//  list. *previousPtr is updated to the value of the new node
void insertNode(node ** previousPtr, int power, float coeff)
{
    node * previous = *previousPtr;

    // Validate previous
    assert(previous->next == NULL);

    // Create node
    node * current = malloc(sizeof(node));
    current->power = power;
    current->coeff = coeff;

    // Add to the end of the list and update previousPtr
    previous->next = current;
    *previousPtr = current;
}

// Read a polynomial from standard input
node * readPolynomial(void)
{
    char line[256];
    node dummyHead = { 0, 0, NULL };
    node * previous = &dummyHead;

    // Get number of powers
    int terms;
    printf("Enter number of terms: ");

    if (fgets(line, sizeof(line), stdin) == NULL)
        terms = 0;
    else
        terms = atoi(line);

    // Read them in
    printf("Enter terms in the form <power> <coefficient> - one on each line\n");
    for (; terms > 0; terms--)
    {
        // Read line
        if (fgets(line, sizeof(line), stdin) == NULL)
            break;

        // Read numbers
        int power;
        float coeff;

        if (sscanf(line, "%d %f", &power, &coeff) == 2)
            insertNode(&previous, power, coeff);
    }

    // Replace empty list with a zero
    if (dummyHead.next == NULL)
        insertNode(&previous, 0, 0);

    // Terminate list
    if (previous != NULL)
        previous->next = NULL;

    return dummyHead.next;
}

// Adds one polynomial to another and generates a new polynomial
//  to represent the result
node * addPolynomial(node * a, node * b)
{
    node dummyHead = { 0, 0, NULL };
    node * previous = &dummyHead;

    while (a != NULL && b != NULL)
    {
        int newPower;
        float newCoeff;

        // Same power?
        if (a->power == b->power)
        {
            // Add coefficients together
            newPower = a->power;
            newCoeff = a->coeff + b->coeff;

            // Advance both
            a = a->next;
            b = b->next;
        }
        else if (a->power > b->power)
        {
            // Insert a
            newPower = a->power;
            newCoeff = a->coeff;
            a = a->next;
        }
        else
        {
            // Insert b
            newPower = b->power;
            newCoeff = b->coeff;
            b = b->next;
        }

        // Insert into list
        if (newCoeff != 0)
            insertNode(&previous, newPower, newCoeff);
    }

    // Terminate list
    if (previous != NULL)
        previous->next = NULL;

    return dummyHead.next;
}

// Prints the polynomial beginning with the given head
void printPolynomial(node * head)
{
    node * current = head;

    // Special case of NULL polynomial
    if (head == NULL)
    {
        printf("0\n");
        return;
    }

    while (current != NULL)
    {
        const char * format;

        if (current == head)
            format = "%.2fx^%d";
        else
            format = " + %.2fx^%d";

        printf(format, current->coeff, current->power);
        current = current->next;
    }

    // Final newline
    printf("\n");
}

// Frees a polynomial started by head
void freePolynomial(node * head)
{
    while (head != NULL)
    {
        node * next = head->next;
        free(head);
        head = next;
    }
}

int main(void)
{
    // Read in 2 polynomials
    node * polyA = readPolynomial();
    node * polyB = readPolynomial();

    // Print them
    printf("Polynomial A:\n");
    printPolynomial(polyA);
    printf("Polynomial B:\n");
    printPolynomial(polyB);

    // Sum them and print result
    node * sum = addPolynomial(polyA, polyB);
    printf("\nSum:\n");
    printPolynomial(sum);

    // Free polynomials
    freePolynomial(polyA);
    freePolynomial(polyB);
    freePolynomial(sum);
    return 0;
}
