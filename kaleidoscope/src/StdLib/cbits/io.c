#include "io.h"

extern const char *__progname;

// compile all: gcc -fPIC -shared -o /usr/lib/libstdlib.so io.c list.c 

// Print a char from a double
double putchari(int32_t i) {
  putchar((char)i);
  fflush(stdout);
  return i;
}

// print a boolean (int1)
 int8_t printb(int8_t b) {
   printf("%s\n", b ? "true" : "false");
   return b;
 }

// print an int list
int32_t printil(intList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  intList *current = list;
  intList *next = NULL;
  // improvement: construct the string and print it in one go
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
int32_t printfl(doubleList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  doubleList *current = list;
  doubleList *next = NULL;
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
int32_t printbl(boolList *list) {
  if (list == NULL) {
    printf("[]\n");
    return 0;
  }
  boolList *current = list;
  boolList *next = NULL;
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