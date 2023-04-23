def pow(base exponent) if exponent < 1 then 1 else base * pow(base, exponent - 1);
pow(4, 3);