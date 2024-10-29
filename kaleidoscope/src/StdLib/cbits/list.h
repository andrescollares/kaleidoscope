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

struct intList *_alloc_int_list_node();

struct doubleList *_alloc_double_list_node();

struct boolList *_alloc_bool_list_node();