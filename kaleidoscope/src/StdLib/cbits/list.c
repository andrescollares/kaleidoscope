#include "list.h"
#include <stdlib.h>

// allocate a new list node
struct intList *_alloc_int_list_node() {
  struct intList *node = (struct intList *)malloc(sizeof(struct intList));
  node->val = 0;
  node->next = NULL;
  return node;
}

struct doubleList *_alloc_double_list_node() {
  struct doubleList *node = (struct doubleList *)malloc(sizeof(struct doubleList));
  node->val = 0;
  node->next = NULL;
  return node;
}

struct boolList *_alloc_bool_list_node() {
  struct boolList *node = (struct boolList *)malloc(sizeof(struct boolList));
  node->val = 0;
  node->next = NULL;
  return node;
}