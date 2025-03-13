#include "list.h"
#include <stdlib.h>

// allocate a new list node
intList *_alloc_int_list_node() {
  intList *node = (intList *)malloc(sizeof(intList));
  return node;
}

doubleList *_alloc_double_list_node() {
  doubleList *node = (doubleList *)malloc(sizeof(doubleList));
  return node;
}

boolList *_alloc_bool_list_node() {
  boolList *node = (boolList *)malloc(sizeof(boolList));
  return node;
}