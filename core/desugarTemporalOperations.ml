open CommonTypes
open Sugartypes

class desugar_operation env =
object (o: 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
    let open TemporalOperation in
    function
      | TemporalOp (Accessor (_, field), phr, _) as phrn ->
          (* Translation is type-preserving, so it's safe to defer to the
           * supertype's computation of the type *)
          let (_, _, ty) = super#phrasenode phrn in
          let (o, ty) = o#datatype ty in
          let (o, phr, _) = o#phrase phr in
          let field_name = TemporalOperation.field field in
          (o, Projection (phr, field_name), ty)
      | pn -> super#phrasenode pn
end

let desugar_operation env =
  ((new desugar_operation env) : desugar_operation :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "temporal operations"
        let obj env =
          (desugar_operation env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
