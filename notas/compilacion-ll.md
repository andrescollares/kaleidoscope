# Compilacion de archivos LLVM IR (.ll)

## Simple

```
gcc id.ll -o id
./id
```

## Con librerias dinamicas (.so)

### Primero, generar un archivo .so, desde un archivo putchard.c

Putchard.ll:

```
; ModuleID = 'Kaleidoscope'
source_filename = "<string>"

declare double @putchard(double)

define double @main() {
entry:
  %0 = call double @putchard(double 1.200000e+02)
  ret double %0
}
```
Putchard.c (libreria)

```
/* putchard.c
$ gcc -fPIC -shared putchard.c -o libputchard.so
*/

#include "stdio.h"

// putchard - putchar that takes a double and returns 0.
double putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}
```

- Importante: usar el prefijo 'lib' para el nombre del SO (shared object)

- Establecer el path para que el sistema busque la libreria

```
LD_LIBRARY_PATH=/Users/fing/kaleidoscope/kaleidoscope/src/lib
DYLD_LIBRARY_PATH=/Users/fing/kaleidoscope/kaleidoscope/src/lib
```

- Compilar con gcc

`gcc -L/Users/fing/kaleidoscope/kaleidoscope/src/lib -Wall -o test putchard.ll -lputchard`

`./test`

