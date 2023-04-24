extern printd(x);

# Operator with precedence 1 that just returns 0
# used for executing 2 intructions one after the other
def binary : 1 (x y) 0;

printd(1) : printd(2) : printd(3);