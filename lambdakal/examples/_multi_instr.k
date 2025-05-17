def binary : 1 (x y) y;

def circlearea(r) 
    var pi = 3.14 in
    pi * r * r;

var dos = 10000 in
    dos + circlearea(2) :
    circlearea(dos);

# problem
circlearea(1);