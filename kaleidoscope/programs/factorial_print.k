extern printd(x);
def factorial(x) if x < 1 then 1 else x * printd(factorial(x - 1));
factorial(8);