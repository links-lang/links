%{
let unparse_label = function
  | `Char c -> String.make 1 c
  | `List (`Char _::_) as s -> Result.unbox_string s
  | r -> (failwith "(json) error decoding label " ^ Result.Show_result.show r)

%}

%token LBRACE RBRACE COLON COMMA LBRACKET RBRACKET TRUE FALSE NULL 
%token <string> STRING
%token <Num.num> INT
%token <float> FLOAT

%start parse_json
%type <Result.result> parse_json

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
                                        let driver = Result.unbox_string (List.assoc "driver" bs)
                                        and params =
                                          Result.reconstruct_db_string
                                            (Result.unbox_string (List.assoc "name" bs),
                                             Result.unbox_string (List.assoc "args" bs)) in
                                          `Database (Result.db_connect driver params) 
(*
                            | ["_params", params; "_driver", driver] -> `Database (Result.db_connect
                                                                                     (Result.unbox_string driver)
                                                                                     (Result.unbox_string params))
*)
                                    | _ -> failwith ("database value must be a record")
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
| string                             { $1 }
| number                             { $1 }
| object_                            { $1 }
| array                              { $1 }
| TRUE                               { `Bool true }
| FALSE                              { `Bool false }
| NULL                               { `Record [] (* Or an error? *) } 

string:
| STRING                             { if String.length $1 == 1 then Result.char (String.get $1 0)
                                       else Result.box_string $1 }

id:
| STRING                             { $1 }

number:
| FLOAT                             { `Float $1 }
| INT                               { `Int $1 }
