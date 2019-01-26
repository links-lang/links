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
end

module Utility : sig
  include module type of struct include LensUtility end
end

module Types : sig
  include module type of LensTypes
end
