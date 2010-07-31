%{
open Utility

let unparse_label = function
  | `Char c -> String.make 1 c
  | `List (`Char _::_) as s -> Value.unbox_string s
  | r -> (failwith "(json) error decoding label " ^ Show.show Value.show_t r)

%}

%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN
%token COLON COMMA UNDERSCORE TRUE FALSE NULL 
%token CLOSURETABLE SERVERFUNC
%token <string> STRING
%token <Num.num> INT
%token <float> FLOAT

%start parse_json
%type <Value.t> parse_json

%% 

parse_json:
| value { $1 }

object_:
| LBRACE RBRACE         { `Record [] }
| LBRACE members RBRACE { match $2 with 
                            | ["_label", l; "_value", v]
                            | ["_value", v; "_label", l] -> `Variant (unparse_label l, v)
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
                                        in
                                          `Table (db, name, row)
                                    | _ -> failwith ("jsonparse: table value must be a record")
                                end
                            | ["_xml", t] ->
                                let unbox_string_or_char r =
                                  match r with
                                    | `List _ -> Value.unbox_string r
                                    | `Char c -> String.make 1 c
                                    | _ -> failwith ("Cannot unbox '"^ Value.string_of_value r ^"' as a string") in
                                  begin
                                    match t with
                                      | `List [node_type; s] when (Value.unbox_string node_type = "TEXT") ->
                                          `XML (Value.Text (unbox_string_or_char s))
                                      | `List [node_type; tag; attrs; body]
                                          when (Value.unbox_string node_type = "ELEMENT") ->
                                          let tag = unbox_string_or_char tag in
                                          let attrs =
                                            match attrs with
                                              | `Record attrs -> attrs
                                              | _ -> failwith ("jsonparse: xml attributes should be an attribute record") in
                                            let attrs =
                                              List.fold_left
                                                (fun attrs (label, value) ->
                                                   Value.Attr (label, unbox_string_or_char value) :: attrs)
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
| func                               { $1 }
| string                             { $1 }
| number                             { $1 }
| object_                            { $1 }
| array                              { $1 }
| TRUE                               { `Bool true }
| FALSE                              { `Bool false }
| NULL                               { `Record [] (* Or an error? *) } 

func:
| CLOSURETABLE LBRACKET INT RBRACKET { `ClientFunction("_closureTable["^
                                                       Num.string_of_num $3^"]")
                                     }
| SERVERFUNC LBRACKET UNDERSCORE INT RBRACKET LBRACE RBRACE
                                     { (* The underscore here is a nuisance; would be good to remove it. *)
                                       `FunctionPtr(Num.int_of_num $4,
                                                    Value.empty_env (Utility.IntMap.empty)) }
| SERVERFUNC LBRACKET UNDERSCORE INT RBRACKET LBRACE members RBRACE
                                     { `FunctionPtr(Num.int_of_num $4,
                                                    Value.extend (Value.empty_env Utility.IntMap.empty) 
                                                      (Utility.IntMap.from_alist
                                                        (List.map (fun (x,y) ->
                                                            (int_of_string x,
                                                                (y,`Local))) $7))) }

string:
| STRING                             { if String.length $1 == 1 then Value.box_char (String.get $1 0)
                                       else Value.box_string $1 }
id:
| STRING                             { $1 }

number:
| FLOAT                             { `Float $1 }
| INT                               { `Int $1 }
