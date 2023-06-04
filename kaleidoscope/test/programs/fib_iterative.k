extern printd(x);
def binary : 1 (x y) y; # define the (:) operator to return the second argument, this is very important ;)
def fibi(x)
  var a = 1, b = 1, c = 0 in
  (for i = 3, i < x, 1.0 in 
    c = (a + b) : 
    a = b : 
    b = c :
    printd(b)
  ): b;

fibi(20);