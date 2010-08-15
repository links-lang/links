(* Alias environment *)
module AliasEnv = Env.String

let alias_env : Types.tycon_environment =
  List.fold_left 
    AliasEnv.bind
    AliasEnv.empty
    [
      "Xml"     , `Alias ([], `Application (Types.list, [`Type (`Primitive `XmlItem)]));
      "Event"   , `Abstract Types.event;
      "List"    , `Abstract Types.list;
      "Process" , `Abstract Types.process;
      "DomNode" , `Abstract Types.dom_node;
    ]

