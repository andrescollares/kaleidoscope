#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "list.h"

double putchari(int32_t i);

int32_t printil(intList *list);

int32_t printfl(doubleList *list);

int32_t printbl(boolList *list);

double writed(double d);

int32_t K_INT = 1;
int32_t K_DOUBLE = 2;
int32_t K_BOOL = 3;