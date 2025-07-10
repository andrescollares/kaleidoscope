def list_max([int] l, int max) -> int:
    if l == [] then
        max
    else
        if (head l) > max then
            list_max(tail l, head l)
        else
            list_max(tail l, max);

list_max([1, 2, 3, 4, 5, 60, 7, 8, 9, 10], 0);