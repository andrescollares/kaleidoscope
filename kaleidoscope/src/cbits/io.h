#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

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

int32_t K_INT = 1;
int32_t K_DOUBLE = 2;
int32_t K_BOOL = 3;

double putchard(double x);

double printd(double d);

int32_t printi(int32_t i);

int8_t printb(int8_t b);

int32_t print_tuple(struct intTuple t, int32_t first_type, int32_t second_type);

int32_t printil(struct intList *list);

int32_t printfl(struct doubleList *list);

int32_t printbl(struct boolList *list);

struct intList *_alloc_int_list_node();

struct doubleList *_alloc_double_list_node();

struct boolList *_alloc_bool_list_node();

double writed(double d);