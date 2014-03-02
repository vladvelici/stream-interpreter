/* File parser.mly */

%{ open InterpreterObjects %}

%token <int> INT
%token <string> VARNAME
%token <InterpreterObjects.tipe> TYPE
%token ASSIGN TYPE_ASSIGN
%token LPAREN RPAREN LBRACE RBRACE
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
    | declaration                       { $1 }
    | VARNAME ASSIGN expr               { Assignment ($1, $3) }
    | VARNAME LPAREN vallist RPAREN     { ApplyFunction ($1, $3) }
    | lambda LPAREN vallist RPAREN      { ApplyLambda ($1, $3) }
    | VARNAME                           { VarName $1 } 
;

/* Matches the following:
 * int a = 3
 * int a
 * a := 3
 * func <name> (<arg>) <retType> { <body> }
*/

declaration:
    | typematch VARNAME ASSIGN expr     { DeclAssign ($2, $1, $4) }
    | typematch VARNAME                 { DeclAssign ($2, $1, Primitive Undefined) }
    | VARNAME TYPE_ASSIGN expr          { CtxDeclaration ($1, $3) }
    | funcexpr                          { $1 }
;


/* primitives */
primitive:
    | INT           { ValInt $1 }
    | lambda        { ValFunction ($1) }
;

/* Sequence of expressions, used in functions */
exprSeq:
    | EOL exprSeq       { $2 }
    | expr EOL exprSeq  { $1 :: $3 }
    | expr              { [$1] }
    |                   { [] }
;

/* matches a type (return InterpreterObjects.tipe) */
typematch:
    | FUNC LPAREN typelist RPAREN typematch         { Function ($5, $3) }
    | FUNC LPAREN RPAREN typematch                  { Function ($4, []) }
    | TYPE                                          { $1 }
;

/* list of types */
typelist:
    | typematch COMMA typelist  { $1 :: $3 }
    | typematch                 { [$1] }
    |                           { [] }
;
/* list of arguments. An argument is a pair (type, varname) */
arglist:
    | typematch VARNAME COMMA arglist   { (Argument ($2, $1)) :: $4 }
    | typematch VARNAME                 { [(Argument ($2, $1))] }
    |                                   { [] }
;

/* list of expressions, used for calling a function */
vallist:
    | expr COMMA vallist    { $1 :: $3 }
    | expr                  { [$1] }
    |                       { [] }
;

/* matches things like func <function name>(<arguments>) <return type> { <some-body> } */
funcexpr:
    | FUNC VARNAME LPAREN arglist RPAREN typematch LBRACE exprSeq RBRACE {
            CtxDeclaration ($2, (Primitive (ValFunction (Func ($6, $4, $8)))))
        }
;

/* matches lambda expressions: func(<arguments>) <return type> { <some body> }*/
lambda:
     | FUNC LPAREN arglist RPAREN typematch LBRACE exprSeq RBRACE { Func($5, $3, $7) }
     | FUNC LPAREN RPAREN typematch LBRACE exprSeq RBRACE { Func ($4, [], $6) }
;
