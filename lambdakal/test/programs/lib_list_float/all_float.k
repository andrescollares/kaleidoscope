def is_whole(double d) -> bool:
  int_to_double(double_to_int(d)) == d;

all_double([1.0, 0.0, 2.0], is_whole);