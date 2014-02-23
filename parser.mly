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
    | typematch VARNAME ASSIGN expr     { DeclAssign $2, $1, $4 }
    | typematch VARNAME                 { DeclAssign $2, $1, Undefined }
    | VARNAME TYPE_ASSIGN expr          { CtxDeclaration $1, $3 }
    | VARNAME ASSIGN expr               { Assignment $1, $3 }
    | callable LPAREN vallist RPAREN    { ApplyFunction $1, $3 }
    | LPAREN expr RPAREN                { $2 }
;

primitive:
    | funcexpr      { $1 }
    | INT           { $1 }
;

exprSeq:
    | expr EOL exprSeq  { Sequence $1, $3 }
    | expr              { Sequence $1, (None:expSequence) }
    |                   { (None:expSequence) }
;

typematch:
    | FUNC LPAREN typelist RPAREN typematch        { Function $5, $3 }
    | TYPE                                          { $1 }
;

typelist:
    | typematch COMMA typelist  { $1 :: $3 }
    | typematch                 { [$1] }
    |                           { [] }
;

arglist:
    | typematch VARNAME COMMA arglist   { (Argument $2, $1) :: $4 }
    | typematch VARNAME                 { [(Argument $2, $1)] }
    |                                   { [] }
;

vallist:
    | expr COMMA vallist    { $1 :: $3 }
    | expr                  { [$1] }
    |                       { [] }
;

callable:
    | funcexpr  { $1 }
    | VARNAME   { $1 }
;

funcexpr:
 | FUNC VARNAME LPAREN arglist RPAREN typematch LBRACKET exprSeq RBRACKET {
        CtxDeclaration $2, (Primitive (ValFunction (Func $6, $4, $8)))
    }
 | FUNC LPAREN arglist RPAREN typematch LBRACKET exprSeq RBRACKET {
        Primitive (ValFunction (Func $5, $3, $7))
    }

