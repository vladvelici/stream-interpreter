/* File parser.mly */

%{ open InterpreterObjects %}

%token <int> INT
%token <string> VARNAME
%token <InterpreterObjects.Type> TYPE
$token ASSIGN TYPE_ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET
%token FUNC
%left PLUS MINUS        /* lowest precedence */
%left MODULO
%left TIMES DIV       /* medium precedence */
%left EXPONENTIAL
%token EOL
%start main             /* the entry point */
%type <InterpreterObjects.Expression> main
%%
main:
        expr EOL                { $1 }
;

expr:
        | TYPEMATCH
        | funcexpr                      { Assignment Expression (Primitive (VarValue $1)) }
        | LPAREN expr RPAREN            { $2 }
        | expr expr                     { Seq ($1, $2) }
;

typematch:
        | FUNC LPARENT typelist RPAREN typematch        { Func $5 $3 }
        | TYPE                                          { $1 }
;

typelist:
 | typematch COMMA typelist { ArgTypes $1, $3 }
 | typematch { ArgTypes $1, None }
 | _ { None }
;

funcexpr:
 | FUNC VARNAME LPARAN varlist RPARAN TYPE LBRACKET expr RBRACKET { Function $2 $4 $7 }
