#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Data structure storing the matrix
typedef struct matrix
{
    unsigned width;
    unsigned height;
    int data[];

} matrix;

// Creates a matrix of the given size and initializes it from a buffer
//  Buffer is an array of width * height integers
//  If buf is NULL, the matrix is not initialized
matrix * matrix_create_data(unsigned width, unsigned height, const int * buf)
{
    // Allocate matrix
    size_t dataSize = (sizeof(int) * width * height);
    matrix * mtx = malloc(sizeof(matrix) + dataSize);

    if (mtx == NULL)
        return NULL;

    // Store size
    mtx->width = width;
    mtx->height = height;

    // Copy buffer
    if (buf != NULL)
        memcpy(mtx->data, buf, dataSize);

    return mtx;
}

// Creates a matrix of the given size
//  DOES NOT INITIALIZE THE VALUES!
matrix * matrix_create(unsigned width, unsigned height)
{
    return matrix_create_data(width, height, NULL);
}

// Creates a copy of an existing matrix
matrix * matrix_copy(matrix * mtx)
{
    return matrix_create_data(mtx->width, mtx->height, mtx->data);
}

// Frees a matrix
void matrix_free(matrix * mtx)
{
    free(mtx);
}

// Clears a matrix so all the elements are null
void matrix_clear(matrix * mtx)
{
    memset(mtx->data, 0, mtx->width * mtx->height * sizeof(int));
}

// Gets a pointer to the given position in the matrix
int * matrix_value(matrix * mtx, unsigned x, unsigned y)
{
    assert(x < mtx->width);
    assert(y < mtx->height);

    return &mtx->data[y * mtx->width + x];
}

// Sets the matrix to the identity matrix
//  This works for non-square matrixes by clearing and then
//  setting the main diagonal to 1
void matrix_identity(matrix * mtx)
{
    matrix_clear(mtx);

    for (unsigned i = 0; i < mtx->width && i < mtx->height; i++)
        *matrix_value(mtx, i, i) = 1;
}

// Adds two matricies together in place
//  The result is stored in mtxA
//  Matricies must be the same size
void matrix_add_inplace(matrix * mtxA, matrix * mtxB)
{
    // Validate sizes
    assert(mtxA->width == mtxB->width);
    assert(mtxA->height == mtxB->height);

    // Add together everything
    //  This abuses the fact that we can ignore the dimensions and just
    //  treat data as an array to be added to another
    size_t elements = mtxA->width * mtxA->height;

    for (size_t i = 0; i < elements; i++)
        mtxA->data[i] += mtxB->data[i];
}

// Adds two matricies together, storing the result in a new matrix
matrix * matrix_add(matrix * mtxA, matrix * mtxB)
{
    matrix * resultMtx = matrix_copy(mtxA);

    if (resultMtx != NULL)
        matrix_add_inplace(resultMtx, mtxB);

    return resultMtx;
}

// Multiplies a matrix by a scalar
void matrix_mul_scalar(matrix * mtx, int value)
{
    // Multiply all the elements (ignoring size of matrix)
    size_t elements = mtx->width * mtx->height;

    for (size_t i = 0; i < elements; i++)
        mtx->data[i] *= value;
}

// Negates a matrix
void matrix_negate(matrix * mtx)
{
    matrix_mul_scalar(mtx, -1);
}

// Transposes the given matrix into a new matrix
matrix * matrix_transpose(matrix * mtx)
{
    // Create new matrix with inverted width and height
    matrix * newMtx = matrix_create(mtx->height, mtx->width);

    if (newMtx == NULL)
        return NULL;

    // Copy all the elements with switched x and y values
    for (unsigned x = 0; x < mtx->width; x++)
        for (unsigned y = 0; y < mtx->height; y++)
            *matrix_value(newMtx, y, x) = *matrix_value(mtx, x, y);

    return newMtx;
}

// Scans the matrix and generates an array of column widths
//  The array is mtx->width large and each entry contains the
//  number of characters needed to display every number in the column
static unsigned * matrix_print_col_widths(matrix * mtx)
{
    // Find largest number in each column
    unsigned * colWidths = calloc(mtx->width, sizeof(unsigned));

    if (colWidths == NULL)
        return NULL;

    for (unsigned x = 0; x < mtx->width; x++)
    {
        for (unsigned y = 0; y < mtx->height; y++)
        {
            int value = *matrix_value(mtx, x, y);
            unsigned uValue;

            // Handle negative numbers
            if (value < 0)
                uValue = (unsigned) abs(value) * 10;
            else
                uValue = (unsigned) value;

            // If larger, replace
            if (uValue > colWidths[x])
                colWidths[x] = uValue;
        }
    }

    // Compute ceil(log10(col + 1)) of all the columns
    for (unsigned x = 0; x < mtx->width; x++)
    {
        unsigned value = colWidths[x];

        if (value == 0)
            colWidths[x] = 1;
        else
            colWidths[x] = (unsigned) ceil(log10(value + 1));
    }

    // Return result
    return colWidths;
}

// Prints the top / bottom line of the table
static void matrix_print_topline(FILE * stream, unsigned innerWidth)
{
    fputc('+', stream);
    for (; innerWidth > 0; innerWidth--)
        fputc('-', stream);
    fputc('+', stream);
    fputc('\n', stream);
}

// Prints a line of the table
static void matrix_print_line(FILE * stream, matrix * mtx,
                                unsigned * colWidths, unsigned y)
{
    fputc('|', stream);
    fputc(' ', stream);

    // Print each number, right justified and with a space to the right
    for (unsigned x = 0; x < mtx->width; x++)
        fprintf(stream, "%*d ", colWidths[x], *matrix_value(mtx, x, y));

    fputc('|', stream);
    fputc('\n', stream);
}

// Prints a matrix to the given stream
void matrix_print(FILE * stream, matrix * mtx)
{
    // Get width of all the columns
    unsigned * colWidths = matrix_print_col_widths(mtx);

    if (colWidths == NULL)
    {
        fprintf(stderr, "Out of memory\n");
        abort();
    }

    // Get inner width (total width without + signs at the end)
    unsigned innerWidth = mtx->width + 1;

    for (unsigned i = 0; i < mtx->width; i++)
        innerWidth += colWidths[i];

    // Print top line
    matrix_print_topline(stream, innerWidth);

    // Print the table itself
    for (unsigned y = 0; y < mtx->height; y++)
        matrix_print_line(stream, mtx, colWidths, y);

    // Print bottom line
    matrix_print_topline(stream, innerWidth);

    // Free colwidths
    free(colWidths);
}

int main(void)
{
    // Matrix testing
    matrix * mtx = matrix_create(5, 5);
    matrix_identity(mtx);

    puts("Identity matrix with -100");
    *matrix_value(mtx, 1, 2) = -100;
    matrix_print(stdout, mtx);

    puts("\nNegated");
    matrix * negated = matrix_copy(mtx);
    matrix_negate(negated);
    matrix_print(stdout, negated);

    puts("\nTransposed");
    matrix * transposed = matrix_transpose(negated);
    matrix_print(stdout, transposed);

    puts("\nAdded with original");
    matrix_add_inplace(transposed, mtx);
    matrix_print(stdout, transposed);

    matrix_free(mtx);
    matrix_free(negated);
    matrix_free(transposed);

    return 0;
}
