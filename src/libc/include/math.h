#ifndef MATH_H
#define MATH_H

double cos(double);
double cosh(double);
double sin(double);
double sinh(double);
double tan(double);
double tanh(double);
double acos(double);
double acosh(double);
double asin(double);
double asinh(double);
double atan(double);
double atanh(double);
double erf(double);
double erfc(double);
double exp(double);
double log(double);
double log2(double);
double log10(double);
double sqrt(double);
double atan2(double, double);
double pow(double, double);
double ceil(double x);
double floor(double x);

int abs(int);
double fabs(double);
long int labs(long int);

int isfinite(int);

#define HUGE_VAL (__builtin_huge_val())

#endif
