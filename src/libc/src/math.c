#include <stub.h>

STUB(cos, (double x), double, 0);
STUB(cosh, (double x), double, 0);
STUB(sin, (double x), double, 0);
STUB(sinh, (double x), double, 0);
STUB(tan, (double x), double, 0);
STUB(tanh, (double x), double, 0);
STUB(acos, (double x), double, 0);
STUB(acosh, (double x), double, 0);
STUB(asin, (double x), double, 0);
STUB(asinh, (double x), double, 0);
STUB(atan, (double x), double, 0);
STUB(atanh, (double x), double, 0);
STUB(erf, (double x), double, 0);
STUB(erfc, (double x), double, 0);
STUB(log, (double x), double, 0);
STUB(log2, (double x), double, 0);
STUB(log10, (double x), double, 0);
STUB(sqrt, (double x), double, 0);
STUB(exp, (double x), double, 0);
STUB(fabs, (double x), double, 0);
STUB(atan2, (double x, double y), double, 0);
STUB(pow, (double x, double y), double, 0);

int abs(int i) {
	return (i < 0 ? -i : i);
}

long labs(long i) {
	return (i < 0 ? -i : i);
}

STUB(ceil, (double x), double, 0);
STUB(floor, (double x), double, 0);
STUBQ(isfinite, (int x), int, 1);
