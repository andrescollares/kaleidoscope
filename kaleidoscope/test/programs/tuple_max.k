const tuple(int, int) VER (33, 1);

def max(tuple(int, int) x) -> int:
    if ((fst x) > (snd x)) then fst x else snd x;

max(VER);