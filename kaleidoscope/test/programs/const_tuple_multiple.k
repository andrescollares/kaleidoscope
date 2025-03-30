const firstTuple = (1, (2.0, 3));

const secondTuple = ((4.5, 5), 6);

-- problema: reutilizacion de memoria del stack
-- cuando se inicializa la segunda tupla, se sobreescribe la primera
-- store volatile no resuelve esto porque simplemente es una flag para el compilador
-- evita que se borre la memoria pero no que se sobreescriba

(fst firstTuple) + (snd secondTuple);