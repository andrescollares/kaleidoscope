def is_whole(double d) -> bool:
  int_to_double(double_to_int(d)) == d;

any_double([1.23, 0.0, -2.45], is_whole);