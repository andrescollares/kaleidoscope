#include <stdio.h>
#include <string.h>
#include <stdint.h>

struct intList {
  int32_t val;
  struct intList *next;
};

struct doubleList {
  double val;
  struct doubleList *next;
};

struct boolList {
  int8_t val;
  struct boolList *next;
};

struct intTuple {
  int32_t a;
  int32_t b;
};

double putchard(double x);

double printd(double d);

int32_t printi(int32_t i);

int8_t printb(int8_t b);

int32_t printii(struct intTuple t);

int32_t printil(struct intList *list);

int32_t printfl(struct doubleList *list);

int32_t printbl(struct boolList *list);

double writed(double d);