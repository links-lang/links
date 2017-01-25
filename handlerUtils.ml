let return_case = "Return"

type depth = Sugartypes.handler_depth
type linearity = [`Linear | `Unrestricted]                        
type sugardescriptor = Sugartypes.hdescriptor
type irdescriptor = (depth * linearity)                                                                       
                    
module SugarDescriptor = struct
  type t = sugardescriptor

  let specialization = function
    | (spec,_) -> spec
             
  let depth desc =
    match specialization desc with
    | (depth,_) -> depth

  let type_info = function
    | (_,ti) -> ti

  let update_type_info ti = function
    | (spec,Some _) 
    | (spec,_)      -> (spec,Some ti)
end

module HandlerDescriptor = struct
   type t = irdescriptor

   let translate_descriptor : SugarDescriptor.t -> t =
     fun desc -> SugarDescriptor.specialization desc

   let depth = fst
   let linearity = snd
end
