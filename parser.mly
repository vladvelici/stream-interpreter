/* File parser.mly */

%{ open InterpreterObjects %}

%token <int> INT
%token <string> VARNAME
%token <InterpreterObjects.tipe> TYPE
%token ASSIGN TYPE_ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET
%token PLUS MINUS MODULO DIV TIMES EXPONENTIAL
%token COMMA
%token FUNC
%left PLUS MINUS        /* lowest precedence */
%left MODULO
%left TIMES DIV       /* medium precedence */
%left EXPONENTIAL
%token EOL
%start main             /* the entry point */
%type <InterpreterObjects.expression> main
%%
main:
    | expr EOL          { $1 }
;

expr:
    | primitive                         { Primitive ($1) }
    | funcexpr                          { $1 }
    | typematch VARNAME ASSIGN expr     { DeclAssign ($2, $1, $4) }
    | typematch VARNAME                 { DeclAssign ($2, $1, Primitive Undefined) }
    | VARNAME TYPE_ASSIGN expr          { CtxDeclaration ($1, $3) }
    | VARNAME ASSIGN expr               { Assignment ($1, $3) }
    | VARNAME LPAREN vallist RPAREN     { ApplyFunction ($1, $3) }
    | lambda LPAREN vallist RPAREN      { ApplyLambda ($1, $3) }
    | LPAREN expr RPAREN                { $2 }
    | VARNAME                           { VarName $1 } 
;

/* primitives excluding functions */
primitive:
    | INT           { ValInt $1 }
;

exprSeq:
    | expr EOL exprSeq  { $1 :: $3 }
    | expr              { [$1] }
    |                   { [] }
;

typematch:
    | FUNC LPAREN typelist RPAREN typematch        { Function ($5, $3) }
    | TYPE                                          { $1 }
;

typelist:
    | typematch COMMA typelist  { $1 :: $3 }
    | typematch                 { [$1] }
    |                           { [] }
;

arglist:
    | typematch VARNAME COMMA arglist   { (Argument ($2, $1)) :: $4 }
    | typematch VARNAME                 { [(Argument ($2, $1))] }
    |                                   { [] }
;

vallist:
    | expr COMMA vallist    { $1 :: $3 }
    | expr                  { [$1] }
    |                       { [] }
;

funcexpr:
    | FUNC VARNAME LPAREN arglist RPAREN typematch LBRACKET exprSeq RBRACKET {
            CtxDeclaration ($2, (Primitive (ValFunction (Func ($6, $4, $8)))))
        }
    | lambda { Primitive (ValFunction ($1)) }
;

lambda:
    | FUNC LPAREN arglist RPAREN typematch LBRACKET exprSeq RBRACKET { Func ($5, $3, $7) }
;
