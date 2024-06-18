#include <stdio.h>
#include <string.h>
#include <stdint.h>

struct intList {
  int32_t val;
  struct intList *next;
};

double putchard(double x);

double printd(double d);

int32_t printi(int32_t i);

int32_t printil(struct intList *list);

double writed(double d);