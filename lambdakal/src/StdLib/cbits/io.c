#include "io.h"

extern const char *__progname;

// compile all: gcc -fPIC -shared -o /usr/lib/libstdlib.so io.c list.c 

// Print a char from a double
double putchari(int32_t i) {
  putchar((char)i);
  fflush(stdout);
  return i;
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

// generates the string representation of an int list to pass to the printf function
// TODO: replace printil with printf & this function

char *intListToString(intList *list) {
  if (list == NULL) {
    return "[]";
  }
  intList *current = list;
  intList *next = NULL;
  char *result = malloc(1024); // allocate a buffer for the string
  strcpy(result, "[");
  while (current != NULL) {
    char buffer[32];
    sprintf(buffer, "%d", current->val);
    strcat(result, buffer);
    next = current->next;
    if (next != NULL) {
      strcat(result, ", ");
    }
    current = next;
  }
  strcat(result, "]");
  return result;
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

char *floatListToString(doubleList *list) {
  if (list == NULL) {
    return "[]";
  }
  doubleList *current = list;
  doubleList *next = NULL;
  char *result = malloc(1024); // allocate a buffer for the string
  strcpy(result, "[");
  while (current != NULL) {
    char buffer[32];
    sprintf(buffer, "%f", current->val);
    strcat(result, buffer);
    next = current->next;
    if (next != NULL) {
      strcat(result, ", ");
    }
    current = next;
  }
  strcat(result, "]");
  return result;
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

char* boolListToString(boolList *list) {
  if (list == NULL) {
    return "[]";
  }
  boolList *current = list;
  boolList *next = NULL;
  char *result = malloc(1024); // allocate a buffer for the string
  strcpy(result, "[");
  while (current != NULL) {
    strcat(result, current->val ? "true" : "false");
    next = current->next;
    if (next != NULL) {
      strcat(result, ", ");
    }
    current = next;
  }
  strcat(result, "]");
  return result;
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