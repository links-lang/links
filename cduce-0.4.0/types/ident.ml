(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module U = Encodings.Utf8

module Id = Ns.QName
type id = Id.t
let ident x = x
let value x = x
let to_string = Id.to_string
let print = Id.print

module IdSet = SortedList.Make(Id)
module IdMap = IdSet.Map
module Env = Map.Make(Id)
type 'a id_map = 'a IdMap.map
type fv = IdSet.t



(* TODO: put following decl somewhere else *)
module Label = Ns.Label
module LabelSet = SortedList.Make(Ns.Label)
module LabelMap = LabelSet.Map

type label = Ns.Label.t
type 'a label_map = 'a LabelMap.map
