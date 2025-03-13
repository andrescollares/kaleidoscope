#include <stdint.h>
#include <stdlib.h>

typedef struct intList {
  int32_t val;
  struct intList *next;
} intList;

typedef struct doubleList {
  double val;
  struct doubleList *next;
} doubleList;

typedef struct boolList {
  int8_t val;
  struct boolList *next;
} boolList;

typedef struct intTuple {
  int32_t a;
  int32_t b;
} intTuple;

intList *_alloc_int_list_node();

doubleList *_alloc_double_list_node();

boolList *_alloc_bool_list_node();