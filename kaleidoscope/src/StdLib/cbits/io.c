#include "io.h"

extern const char *__progname;

// compile all: gcc -fPIC -shared -o /usr/lib/stdlib.so io.c list.c 

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

// print a tuple 
int32_t print_tuple(struct intTuple t, int32_t first_type, int32_t second_type) {
  if (first_type == K_INT) {
    printf("(%d, ", t.a);
  } else if (first_type == K_DOUBLE) {
    printf("(%f, ", t.a);
  } else if (first_type == K_BOOL) {
    printf("(%s, ", t.a ? "true" : "false");
  }

  if (second_type == K_INT) {
    printf("%d)\n", t.b);
  } else if (second_type == K_DOUBLE) {
    printf("%f)\n", t.b);
  } else if (second_type == K_BOOL) {
    printf("%s)\n", t.b ? "true" : "false");
  }

  return 0;
}

// print an int list
int32_t printil(struct intList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  struct intList *current = list;
  struct intList *next = NULL;
  printf("[");
  while (current != NULL) {
    printf("%d", current->val);
    next = current->next;
    if (next != NULL) {
      printf(", ");
    }
    current = next;
  }
  printf("]\n");
}

// print a double list
int32_t printfl(struct doubleList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  struct doubleList *current = list;
  struct doubleList *next = NULL;
  printf("[");
  while (current != NULL) {
    printf("%f", current->val);
    next = current->next;
    if (next != NULL) {
      printf(", ");
    }
    current = next;
  }
  printf("]\n");
}

// print a boolean list
int32_t printbl(struct boolList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  struct boolList *current = list;
  struct boolList *next = NULL;
  printf("[");
  while (current != NULL) {
    printf("%s", current->val ? "true" : "false");
    next = current->next;
    if (next != NULL) {
      printf(", ");
    }
    current = next;
  }
  printf("]\n");
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