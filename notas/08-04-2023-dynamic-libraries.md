# Funciones externas

## Solucion

Kaleidoscope permit invocar funciones externa mediante la keyword `extern`.
Estas funciones no se definend en nuestro lenguage sino que provienend de librerias que estan presentes en el sistema.

Por ejemplo:
```
; ModuleID = 'Kaleidoscope'

declare double @sin(double)

define double @main() {
entry:
  %0 = call double @sin(double 1.000000e+00)
  ret double %0
}
```

Se invoca a la funcion `sin` mediante `dlsym("sin")` en el proceso que esta corriendo Kaleidoscope.

Con esto se puede invocar a librerias arbitrarias in importar en que lenguaje fueron implementadas, esto es de gran utilidad
ya que es posible ejecutar codigo arbitrario directamente desde el interprete JIT de Kaleidoscope.

En particular, se utilizarán funciones implementadas en C para las operaciones de entrada/salida como parte
de la libreria estandar de Kaleidoscope.

Esto requiere que se generen archivos con extension `.so` (shared objects) que deben estar presentes en el entorno
de ejecución para poder utilizar las funciones definidas. La generacion de estos archivos se logra mediante gcc:
`gcc -fPIC -shared /kaleidoscope/src/cbits/putchard.c -o /usr/lib/putchard.so`

Este comando es ejecutado previo a la etapa de build de Kaleidoscope, en la cual se especifica este archivo
como libreria dinamica en los parametros de ghc.


## Debugging

Si la libreria no se encuentra enlazada correctamente, falla con un Segmentation Fault, lo cual es dificil de debuggear.

Mediante las herramientas `ldd` en Linux o `otool -L` en macOS, se pueden ver cuales son las librerias dinamicas enlazadas a un ejecutable.

Mediante Cabal, se pueden generar estas librerias desde un archivo .c, pero no se pudo lograr esto entonces se realizo como un paso extra en el Dockerfile (posible mejora aqui)


Fuentes:
- Capitulo 4 Kaleidoscope
- https://stackoverflow.com/questions/9688200/difference-between-shared-objects-so-static-libraries-a-and-dlls-so
- https://users.cs.utah.edu/~germain/PPS/Topics/C_Language/file_IO.html