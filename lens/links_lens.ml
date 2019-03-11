module Debug = Debug

module Column = Column
module Sort = Sort
module Fun_dep = Fun_dep
module Alias = Alias

module Phrase = struct
  include Lens_phrase

  module Sugar = Lens_phrase_sugar

  module Type = Lens_phrase_type

  module Typesugar = Lens_phrase_typesugar
end

module Operators = Lens_operators

module Type = Lens_type
module Value = Lens_value
module Database = Database
module Statistics = Lens_statistics
module Sorted_records = Sorted_records


module Helpers = struct
  module Classic = LensHelpersClassic
  module Incremental = LensHelpersIncremental
end

module Utility = Lens_utility
