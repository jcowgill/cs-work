#include <stdio.h>
#include <stdlib.h>

typedef struct node
{
    int data;
    struct node * next;

} node;

// Read a list of integers from the given file
//  One integer per line
//  NULL is a valid list (which is empty)
node * readList(FILE * input)
{
    char line[256];
    node dummyHead = { 0, NULL };
    node * previous = &dummyHead;

    // Loop around each line
    while (fgets(line, sizeof(line), input) != NULL)
    {
        // Create new node and store this integer
        node * current = malloc(sizeof(node));
        current->data = atoi(line);

        // Add to the end of the chain
        previous->next = current;
        previous = current;
    }

    // Terminate list
    if (previous != NULL)
        previous->next = NULL;

    return dummyHead.next;
}

// Prints the list beginning with the given head
void printList(node * head, FILE * output)
{
    while (head != NULL)
    {
        fprintf(output, "%d\n", head->data);
        head = head->next;
    }
}

// Frees a list started by head
void freeList(node * head)
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
    // Read, print and free the list
    node * head = readList(stdin);

    printList(head, stdout);
    freeList(head);

    return 0;
}
