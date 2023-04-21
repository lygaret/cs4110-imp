%{
    open Core
%}

%token EOF
%token OPAREN CPAREN OBRACE CBRACE

(* arithmetic *)
%token<string> VAR
%token<int>    INT
%token PLUS STAR

(* boolean *)
%token<bool> BOOL
%token LESS_THAN

(* command *)
%token SKIP PRINT
%token ASSIGN SEMI
%token IF THEN ELSE
%token WHILE DO

%start <cexp> main

%%

let main := 
| e = cexprs; EOF?; { e }

let cexprs :=
| c = cexpr;                   { Seq (c, Skip) }
| a = cexpr; SEMI; b = cexprs; { Seq (a, b) }

let dexprs :=
| OBRACE; c = cexprs; CBRACE;  { c }

let cexpr :=
| SKIP;                                          { Skip }
| PRINT; n = VAR;                                { Print n }
| n = VAR; ASSIGN; a = aexpr;                    { Assign (n, a) }
| IF; p = bexpr; a = dexprs; ELSE; b = dexprs;   { Cond (p, a, b) }
| IF; p = bexpr; a = dexprs;                     { Cond (p, a, Skip) }
| WHILE; p = bexpr; a = dexprs;                  { While (p, a) }

let bexpr :=
| ~ = BOOL;                                        <Bool>
| a = aexpr; LESS_THAN; b = aexpr;                 { LessThan (a, b) }

let aexpr :=
| ~ = VAR;                                         <Var>
| ~ = INT;                                         <Int>
| OPAREN; a = aexpr; CPAREN;                       { a }
| a = aexpr; PLUS; b = aexpr;                      { Add (a, b) }
| a = aexpr; STAR; b = aexpr;                      { Mul (a, b) }