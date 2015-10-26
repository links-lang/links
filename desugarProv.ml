(*
  Desugaring provenance
  ---------------------

*)

let dp = Sugartypes.dummy_position

class desugar_prov env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Data e as dbg ->
       let a, e, c = o#phrase e in
       let e : Sugartypes.phrasenode = `Projection (e, "data") in
       Debug.print ("desugaring " ^ Sugartypes.Show_phrasenode.show dbg ^
                           " to " ^ Sugartypes.Show_phrasenode.show e);
       (o, e, c)
    | `Prov e as dbg ->
       let a, e, c = o#phrase e in
       let e : Sugartypes.phrasenode = `Projection (e, "prov") in
       Debug.print ("desugaring " ^ Sugartypes.Show_phrasenode.show dbg ^
                           " to " ^ Sugartypes.Show_phrasenode.show e);
       (o, e, c)
    | e -> super#phrasenode e
end

let desugar_prov env = ((new desugar_prov env) : desugar_prov :> TransformSugar.transform)
