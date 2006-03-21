%{

(* JSON <-> Result.result *)



let jsonize_primitive : Result.primitive -> string = function
  | `Bool value -> string_of_bool value
  | `Int value -> Num.string_of_num value
  | `Float value -> string_of_float value
  | `Char c -> "'"^ Char.escaped c ^"'"
  | `XML _
  | `PFunction _ as p -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_primitive p); ""

let rec jsonize_result : Result.result -> string = function
  | `Variant _
  | `Database _
  | `Environment _
  | `Continuation _
  | `Collection (`List, (`Primitive(`XML _)::_))
  | `Function _ as r -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_result r); ""
  | `Primitive p -> jsonize_primitive p
  | `Record fields -> "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ " : " ^ jsonize_result v) fields) ^ "}"
  | `Collection (_, []) -> "[]"
  | `Collection (`List, `Primitive(`Char _)::_) as c  -> "\"" ^ Result.escape (Result.charlist_as_string c) ^ "\""
  | `Collection (`List, elems) -> "[" ^ String.concat ", " (List.map jsonize_result elems) ^ "]"
  | r -> prerr_endline ("Can't yet jsonize " ^ Result.string_of_result r); ""

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
| LBRACKET RBRACKET                  { `Collection (`List, []) }
| LBRACKET elements RBRACKET         { `Collection (`List, (List.rev $2)) }

elements:
| value                              { [$1] }
| elements COMMA value               { $3 :: $1 }

value:
| string                             { $1 }
| number                             { $1 }
| object_                            { $1 }
| array                              { $1 }
| TRUE                               { `Primitive (`Bool true) }
| FALSE                              { `Primitive (`Bool false) }
| NULL                               { `Record [] (* Or an error? *) } 

string:
| STRING                             { Result.string_as_charlist $1 }

id:
| STRING                             { $1 }

number:
| FLOAT                             { `Primitive (`Float $1) }
| INT                               { `Primitive (`Int $1) }
