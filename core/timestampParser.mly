%{
open Utility
%}

%token EOF
%token <int> UINTEGER
%token <float> UFLOAT
%token PLUS
%token MINUS
%token COLON
%token INFINITY

%start timestamp
%type < [>`Timestamp of (CalendarShow.t * int option) | `Infinity | `MinusInfinity] > timestamp

%%

date:
| UINTEGER MINUS UINTEGER MINUS UINTEGER                       { $1, $3, $5 }

time:
| UINTEGER COLON UINTEGER COLON float_or_int                   { $1, $3, $5 }

infinity:
| MINUS INFINITY                                        { `MinusInfinity }
| INFINITY                                              { `Infinity }

datetime:
| date time offset? {
    let year, month, day = $1 in
    let hour, minute, second = $2 in
    `Timestamp ((CalendarShow.lmake ~year ~month ~day ~hour ~minute ~second ()), $3) }

timestamp:
| datetime EOF { $1 }
| infinity EOF { $1 }

offset:
| PLUS UINTEGER                                                { $2 }
| MINUS UINTEGER                                               { (-$2) }

float_or_int:
| UFLOAT                                                       { $1 }
| UINTEGER                                                     { float_of_int $1 }
