module Debug = Debug
module Column = Column
module Sort = Sort
module Fun_dep = Fun_dep
module Alias = Alias

module Phrase = struct
  include Phrase
  module Sugar = Phrase_sugar
  module Type = Phrase_type
  module Typesugar = Phrase_typesugar
end

module Operators = Operators
module Type = Type
module Value = Value
module Database = Database
module Statistics = Statistics
module Sorted_records = Sorted_records

module Eval = struct
  include Eval
  module Incremental = Eval_incremental
  module Classic = Eval_classic
end

module Utility = Lens_utility
