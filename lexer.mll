(* File lexer.mll *)
{
open InterpreterObjects
open Parser
exception Eof
}

rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }

    (* primitive type matches, no func and stream *)
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }

    (* assignment syntax *)
    | "="       { ASSIGN }
    | ":="      { TYPE_ASSIGN }

    (* separator *)
    | ','       { COMMA }

    (* brackets *)
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '('       { LPAREN }
    | ')'       { RPAREN }

    (* arithmetics *)
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIV }
    | '^'       { EXPONENTIAL }
    | "sqrt"	{ SQRT }
    | '%'       { MODULO }
    | "abs"	{ ABS }
    | "=="	{ EQUAL }
    | '<'	{ LESS }
    | '>'	{ GREATER }
    | "<="	{ LESSEQUAL }
    | ">="	{ GREATEREQUAL }
    | "||"	{ OR }
    | "&&"	{ AND }

    (* variable types *)
    | "int" | "string" | "float" | "bool" as lxm { TYPE(type_of_string lxm) }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as lxm { VARNAME(lxm) }

    (* keywords *)
    | "true"	{ TRUE }
    | "false"   { FALSE }
    | "if"	{ IF }
    | "else"	{ ELSE }
    | "for"	{ FOR }
    | "while"	{ WHILE }
    | "do-while"	{ DOWHILE }
    | "func"    { FUNC }

    | eof      { raise Eof }
