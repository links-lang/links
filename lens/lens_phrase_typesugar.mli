type 'a error = {
  msg : string;
  data : 'a;
}

val tc_sort : sort:Sort.t -> 'a Lens_phrase_sugar.phrase -> (Lens_phrase_type.t, 'a error) result
