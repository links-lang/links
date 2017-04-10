%{
open Utility
open ProcessTypes

(* let unparse_label = function *)
(*   | `Char c -> String.make 1 c *)
(*   | `List (`Char _::_) as s -> Value.unbox_string s *)
(*   | r -> (failwith "(json) error decoding label " ^ Show.show Value.show_t r) *)

(* BUG: need to unescape strings
   (where they are escaped in json.ml)
*)
%}

%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN
%token COLON COMMA UNDERSCORE TRUE FALSE NULL
%token <string> STRING
%token <int> INT
%token <float> FLOAT

%start parse_json
%type <Value.t> parse_json

%%

parse_json:
| value { $1 }

object_:
| LBRACE RBRACE         { `Record [] }
| LBRACE members RBRACE { match $2 with
                            | ["_c", c] -> Value.box_char ((Value.unbox_string c).[0])
                            | ["_label", l; "_value", v]
                            | ["_value", v; "_label", l] -> `Variant (Value.unbox_string l, v)
                            | ["_clientAPID", apid_str; "_clientId", cid_str]
                            | ["_clientId", cid_str; "_clientAPID", apid_str] ->
                                let apid = AccessPointID.of_string @@ Value.unbox_string apid_str in
                                let cid = ClientID.of_string @@ Value.unbox_string cid_str in
                                `AccessPointID (`ClientAccessPoint (cid, apid))
                            | ["_serverAPID", apid_str] ->
                                let apid = AccessPointID.of_string @@ Value.unbox_string apid_str in
                                `AccessPointID (`ServerAccessPoint (apid))
                            | ["_serverPid", v] ->
                                `Pid (`ServerPid (ProcessID.of_string (Value.unbox_string v)))
                            | ["_clientPid", pid_str; "_clientId", client_id_str]
                            | ["_clientId", client_id_str; "_clientPid", pid_str] ->
                                let client_id = ClientID.of_string (Value.unbox_string client_id_str) in
                                let pid = ProcessID.of_string (Value.unbox_string pid_str) in
                                `Pid (`ClientPid (client_id, pid))
                            | ["_clientSpawnLoc", client_id_str] ->
                                let client_id = ClientID.of_string (Value.unbox_string client_id_str) in
                                `SpawnLocation (`ClientSpawnLoc (client_id))
                            | ["_serverSpawnLoc", _] ->
                                `SpawnLocation (`ServerSpawnLoc)
                            | ["_sessEP1", ep1_str; "_sessEP2", ep2_str]
                            | ["_sessEP2", ep2_str; "_sessEP1", ep1_str] ->
                                let ep1 = ChannelID.of_string @@ Value.unbox_string ep1_str in
                                let ep2 = ChannelID.of_string @@ Value.unbox_string ep2_str in
                                `SessionChannel (ep1, ep2)
                            | ["_db", db] ->
                                begin
                                  match db with
                                    | `Record bs ->
                                        let driver = Value.unbox_string (List.assoc "driver" bs)
                                        and params =
                                          Value.reconstruct_db_string
                                            (Value.unbox_string (List.assoc "name" bs),
                                             Value.unbox_string (List.assoc "args" bs)) in
                                          `Database (Value.db_connect driver params)
                                    | _ -> failwith ("jsonparse: database value must be a record")
                                end
                            | ["_table", t] ->
                                begin
                                  match t with
                                    | `Record bs ->
                                        let db =
                                          begin
                                            match List.assoc "db" bs with
                                              | `Database db -> db
                                              | _ -> failwith ("jsonparse: first argument to a table must be a database")
                                          end
					and name = Value.unbox_string (List.assoc "name" bs)
                                        and row =
                                          begin
                                            match DesugarDatatypes.read ~aliases:Env.String.empty (Value.unbox_string (List.assoc "row" bs)) with
                                                | `Record row -> row
                                                | _ -> failwith ("jsonparse: tables must have record type")
                                          end
                                        and keys =
					  begin
					    match List.assoc "keys" bs with
					      | `List keys ->
						  List.map
						    (function
						       | `List part_keys ->
							   List.map Value.unbox_string part_keys
						       | _ -> failwith "jsonparse: keys must be lists of strings")
						    keys
					      | _ -> failwith ("jsonparse: table keys must have list type")
					  end
                                        in
                                          `Table (db, name, keys, row)
                                    | _ -> failwith ("jsonparse: table value must be a record")
                                end
                            | ["_xml", t] ->
                                  begin
                                    match t with
                                      | `List [node_type; s] when (Value.unbox_string node_type = "TEXT") ->
                                          `XML (Value.Text (Value.unbox_string s))
                                      | `List [node_type; tag; attrs; body]
                                          when (Value.unbox_string node_type = "ELEMENT") ->
                                          let tag = Value.unbox_string tag in
                                          let attrs =
                                            match attrs with
                                              | `Record attrs -> attrs
                                              | _ -> failwith ("jsonparse: xml attributes should be an attribute record") in
                                            let attrs =
                                              List.fold_left
                                                (fun attrs (label, value) ->
                                                   Value.Attr (label, Value.unbox_string value) :: attrs)
                                                [] attrs in
                                              let body =
                                                match body with
                                                  | `List body -> List.map
                                                      (function
                                                         | `XML body -> body
                                                         | _ -> failwith ("jsonparse: xml body should be a list of xmlitems"))
                                                        body
                                                  | _ -> failwith ("jsonparse: xml body should be a list of xmlitems")
                                              in
                                                `XML (Value.Node (tag, attrs @ body))
                                      | _ ->
                                          failwith ("jsonparse: xml should be either a text node or an element node. Got: "
                                                    ^ Value.string_of_value t)
                                  end
                            | ["_closureTable", id] ->
                              `ClientFunction("_closureTable["^Value.string_of_value id^"]")
                            | ["_serverFunc", id]
                            | ["_serverFunc", id; "_env", `Record []]
                            | ["_env", `Record []; "_serverFunc", id] ->
                              `FunctionPtr(Value.unbox_int id, None)
                            | ["_serverFunc", id; "_env", fvs]
                            | ["_env", fvs; "_serverFunc", id] ->
                              `FunctionPtr(Value.unbox_int id, Some fvs)
                            | _ -> `Record (List.rev $2)
                        }

members:
| id COLON value                     { [$1, $3] }
| members COMMA id  COLON value      { ($3, $5) :: $1 }

array:
| LBRACKET RBRACKET                  { `List ([]) }
| LBRACKET elements RBRACKET         { `List (List.rev $2) }

elements:
| value                              { [$1] }
| elements COMMA value               { $3 :: $1 }

value:
| string                             { $1 }
| number                             { $1 }
| object_                            { $1 }
| array                              { $1 }
| TRUE                               { `Bool true }
| FALSE                              { `Bool false }
| NULL                               { `Record [] (* Or an error? *) }

string:
| STRING                             { Value.box_string $1 }
id:
| STRING                             { $1 }

number:
| FLOAT                             { `Float $1 }
| INT                               { `Int $1 }
