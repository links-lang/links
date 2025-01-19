type 'a error = { msg : string; data : 'a }

val tc_columns :
  columns:Column.t list ->
  'a Phrase_sugar.phrase ->
  (Phrase_type.t, 'a error) result
