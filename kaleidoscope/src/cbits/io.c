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


