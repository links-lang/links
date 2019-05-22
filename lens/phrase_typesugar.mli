type 'a error = {msg: string; data: 'a}

val tc_sort :
  sort:Sort.t -> 'a Phrase_sugar.phrase -> (Phrase_type.t, 'a error) result
