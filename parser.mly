/* File parser.mly */

%{ open InterpreterObjects %}

%token <int> INT
%token <float> FLOAT
%token <string> VARNAME
%token <InterpreterObjects.tipe> TYPE
%token ASSIGN TYPE_ASSIGN
%token COMMA
%token LBRACE RBRACE LPAREN RPAREN
%token PLUS MINUS TIMES DIV EXPONENTIAL MODULO ABS 
%token EQUAL LESS GREATER LESSEQUAL GREATEREQUAL NONEQUAL NOT
%token OR AND
%token COLON TILDE LCHEVRONS
%token TRUE FALSE
%token IF ELSE
%token FOR WHILE DO WHILE
%token FUNC
%token STREAM
%token EOL
%token NULL UNDEFINED

%left EQUAL LESS GREATER NONEQUAL LESSEQUAL GREATEREQUAL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV MODULO       /* medium precedence */
%left EXPONENTIAL

%nonassoc NOT
%nonassoc UMINUS
%nonassoc LCHEVRONS
%nonassoc TILDE

%start main             /* the entry point */

%type <InterpreterObjects.output> main
%%
main:
    | EOL               { Empty }
    | expr EOL          { Expression $1 }
;

/* Expression */
expr:
    | primitive                         { Primitive ($1) }
    | numerical                         { $1 }
    | if_statement                      { $1 }
    | loop                              { $1 }
    | declaration                       { $1 }
    | VARNAME ASSIGN expr               { Assignment ($1, $3) }
    | VARNAME LPAREN vallist RPAREN     { ApplyFunction ($1, $3) }
    | lambda LPAREN vallist RPAREN      { ApplyLambda ($1, $3) }
    | VARNAME                           { VarName $1 } 
    | streams                           { $1 }
    | LPAREN expr RPAREN                { $2 }
;

/* primitives */
primitive:
    | INT           { ValInt $1 }
    | FLOAT         { ValFloat $1 }
    | TRUE          { ValBoolean true }
    | FALSE         { ValBoolean false }
    | lambda        { ValFunction ($1) }
    | NULL          { Null }
    | UNDEFINED     { Undefined }
;

/* Numerical operations */
numerical:
    /* Arithmetics */
    | expr PLUS expr            { PlusOperator ($1, $3) }
    | expr MINUS expr           { MinusOperator ($1, $3) }
    | expr TIMES expr           { MultiplyOperator ($1, $3) }
    | expr DIV expr             { DivOperator ($1, $3) }
    | expr EXPONENTIAL expr     { ExponentOperator ($1, $3) }
    | expr MODULO expr          { ModOperator ($1, $3) }
    | MINUS expr %prec UMINUS   { NegationOperator ($2) }

    /* Equality testing */
    | expr EQUAL expr           { Equal ($1, $3) }
    | expr NONEQUAL expr        { NonEqual ($1, $3) }

    /* Order relations */
    | expr LESS expr            { Less ($1, $3) }
    | expr GREATER expr         { Greater ($1, $3) }
    | expr LESSEQUAL expr       { LessEqual ($1, $3) }
    | expr GREATEREQUAL expr    { GreaterEqual ($1, $3) }

    /* Boolean logic */
    | NOT expr              { Not ($2) }
    | expr OR expr          { Or ($1, $3) }
    | expr AND expr         { And ($1, $3) }

/* If statement */
if_statement:
   | IF LPAREN expr RPAREN LBRACE exprSeq RBRACE                            { If ($3, $6) }
   | IF LPAREN expr RPAREN LBRACE exprSeq RBRACE ELSE LBRACE exprSeq RBRACE { IfElse ($3, $6, $10) }

/* Loops */
loop:
   | FOR LPAREN expr COMMA expr COMMA expr RPAREN LBRACE exprSeq RBRACE { ForLoop ($3, $5, $7, $10) }
   | WHILE LPAREN expr RPAREN LBRACE exprSeq RBRACE                     { WhileLoop ($3, $6) }
   | DO LBRACE exprSeq RBRACE WHILE LPAREN expr RPAREN                  { DoWhileLoop ($7, $3) }

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
    | STREAM COLON typematch                        { Stream $3 }
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
     | FUNC LPAREN arglist RPAREN typematch LBRACE exprSeq RBRACE   { Func($5, $3, $7) }
     | FUNC LPAREN RPAREN typematch LBRACE exprSeq RBRACE           { Func ($4, [], $6) }
;

/* Stream arithmetic */
streams:
    | TILDE expr                        { NewStream $2 }
    | LCHEVRONS expr                    { ReadStream $2 }
;

