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
        | typematch VARNAME             { DeclAssign $2 $1 Undefined }
        | typematch VARNAME ASSIGN expr { DeclAssign $2 $1 $4 }
        | VARNAME TYPE_ASSIGN expr      { CtxDeclaration $1 $3 }
        | VARNAME ASSIGN expr           { Assignment $1 $3 }
        | funcexpr                      { $1 }
        | LPAREN expr RPAREN            { $2 }
        | VARNAME LPAREN arglist RPAREN { ApplyFunction $1 $3 }
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
 | FUNC VARNAME LPAREN varlist RPAREN typematch LBRACKET expr RBRACKET {
        CtxDeclaration $2 (Primitive (ValFunction (Function $6 $4 $8)))
    }
 | FUNC LPAREN varlist RPAREN typematch LBRACKET expr RBRACKET {
        Primitive (ValFunction (Function $5 $3 $7))
    }

