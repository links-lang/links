module Helpers : sig
  include module type of struct include LensHelpers end
  module Record : sig
    include module type of struct include LensRecordHelpers end
  end
  module Classic : sig
    include module type of LensHelpersClassic
  end
  module Incremental : sig
    include module type of LensHelpersIncremental
  end
  module Query : sig
    include module type of struct include LensQueryHelpers end
  end
  module FunDeps : sig
    include module type of LensFDHelpers
  end
end

module Utility : sig
  include module type of struct include LensUtility end
end

module Types : sig
  include module type of LensTypes
end

module SetOperations : sig
  include module type of struct include LensSetOperations end
end
