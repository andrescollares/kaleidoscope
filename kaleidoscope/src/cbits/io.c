#include "io.h"

extern const char *__progname;

// Print a char from a double
double putchard(double d) {
  putchar((char)d);
  fflush(stdout);
  return 0;
}

// print a double
double printd(double d) {
  printf("%f\n", d);
  return d;
}

// print an int32
int32_t printi(int32_t i) {
  printf("%d\n", i);
  return i;
}

// print a boolean (int1)
int8_t printb(int8_t b) {
  printf("%s\n", b ? "true" : "false");
  return b;
}

// print a tuple of two ints
// TODO: print any other type
int32_t printii(struct intTuple t) {
  printf("(%d, %d)\n", t.a, t.b);
  return 0;
}

// print an int list
int32_t printil(struct intList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  printf("[");
  _print_int_list(list);
  printf("]\n");
}

int32_t _print_int_list(struct intList *list) {
  struct intList *current = list;
  while (current != NULL) {
    printf("%d ", current->val);
    current = current->next;
  }
  return 0;
}

// print a double list
int32_t printfl(struct doubleList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  printf("[");
  _print_double_list(list);
  printf("]\n");
}

int32_t _print_double_list(struct doubleList *list) {
  struct doubleList *current = list;
  while (current != NULL) {
    printf("%f ", current->val);
    current = current->next;
  }
  return 0;
}

// print a boolean list
int32_t printbl(struct boolList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  printf("[");
  _print_bool_list(list);
  printf("]\n");
}

int32_t _print_bool_list(struct boolList *list) {
  struct boolList *current = list;
  while (current != NULL) {
    printf("%s ", current->val ? "true" : "false");
    current = current->next;
  }
  return 0;
}

// write a double to a file
double writed(double d) {
  FILE *fptr;

  // generate filename as <program name>.out
  char filename[strlen(__progname) + 1 + 4];
  strcpy(filename, __progname);
  strcat(filename, ".out");

  fptr = fopen(filename,"w");
  if(fptr == NULL) {
    printf("Failed to open file for %s", filename);
  } else {
    fprintf(fptr, "%f\n", d);
    fclose(fptr);
  }

  return 0;
}


