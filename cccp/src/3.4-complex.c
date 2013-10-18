#include <stdio.h>

typedef struct complex
{
    double real;    // Real part
    double imag;    // Imaginary part

} complex;

complex cadd(complex a, complex b)
{
    a.real += b.real;
    a.imag += b.imag;
    return a;
}

complex csub(complex a, complex b)
{
    a.real -= b.real;
    a.imag -= b.imag;
    return a;
}

complex cmul(complex a, complex b)
{
    complex new;

    new.real = (a.real * b.real) - (a.imag * b.imag);
    new.imag = (a.real * b.imag) + (a.imag * b.real);

    return new;
}

double getdouble(char letter)
{
    double val;

    printf("%c: ", letter);
    scanf("%lf", &val);
    getchar();

    return val;
}

int main(void)
{
    complex a, b;

    // Read test numbers
    printf("Enter test numbers on each line:\n");
    printf(" (a + bi) and (c + di)\n\n");

    a.real = getdouble('a');
    a.imag = getdouble('b');
    b.real = getdouble('c');
    b.imag = getdouble('d');

    // Calculate results
    complex resultAdd = cadd(a, b);
    complex resultSub = csub(a, b);
    complex resultMul = cmul(a, b);

    // Print results
    printf("Addition = %f + %fi\n", resultAdd.real, resultAdd.imag);
    printf("Subtract = %f + %fi\n", resultSub.real, resultSub.imag);
    printf("Multiply = %f + %fi\n", resultMul.real, resultMul.imag);
    return 0;
}
