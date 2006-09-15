%{

(* JSON <-> Result.result *)



let jsonize_primitive : Result.primitive_value -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML _
  | `Table _
  | `Database _ as p -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_primitive p); ""

let rec jsonize_result : Result.result -> string = function
  | `Variant _
  | `Continuation _
  | `List ((`XML _)::_)
  | `PFunction _ 
  | `Function _ as r -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_result r); ""
  | #Result.primitive_value as p -> jsonize_primitive p
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ " : " ^ jsonize_result v) fields) ^ "}"
  | `List [] -> "[]"
  | `List ((`Char _)::_) as c  -> "\"" ^ Result.escape (Result.charlist_as_string c) ^ "\""
  | `List (elems) -> "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"

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
| LBRACE members RBRACE { `Record (List.rev $2) }

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
| STRING                             { Result.string_as_charlist $1 }

id:
| STRING                             { $1 }

number:
| FLOAT                             { `Float $1 }
| INT                               { `Int $1 }
