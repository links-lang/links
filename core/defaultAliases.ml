open CommonTypes

(* Alias environment *)
module AliasEnv = Env.String

let alias_env : Types.tycon_environment =
  List.fold_left
    (fun env (name, t) ->
      AliasEnv.bind name t env)
    AliasEnv.empty
    [ (* "String"  , `Alias ([], `Application (Types.list, [`Type (`Primitive Primitive.Char)])); *)
      "Xml"     , `Alias ([], Types.Application (Types.list, [(PrimaryKind.Type, Types.Primitive Primitive.XmlItem)]));
      "Event"   , `Abstract Types.event;
      "List"    , `Abstract Types.list;
      "Process" , `Abstract Types.process;
      "DomNode" , `Abstract Types.dom_node;
      "AP"      , `Abstract Types.access_point;
      "EndBang" , `Alias ([], Types.make_endbang_type);
      "Socket"  , `Abstract Types.socket;
      "ValidTime", `Abstract Types.valid_time_data;
      "TransactionTime", `Abstract Types.transaction_time_data;
      "Location", `Abstract Types.spawn_location ]
