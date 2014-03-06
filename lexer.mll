(** File lexer.mll **)
{
open InterpreterObjects
open Parser
exception Eof
}

rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }

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
    | '%'       { MODULO }
    | "=="	{ EQUAL }
    | '<'	{ LESS }
    | '>'	{ GREATER }
    | "<="	{ LESSEQUAL }
    | ">="	{ GREATEREQUAL }
    | "||"	{ OR }
    | "&&"	{ AND }

    (* stream manipulation syntax *)
    | ":"       { COLON }
    | "<<"      { LCHEVRONS }
    | "~"       { TILDE }

    (* keywords *)
    | "true"	{ TRUE }
    | "false"   { FALSE }
    | "if"	{ IF }
    | "else"	{ ELSE }
    | "for"	{ FOR }
    | "while"	{ WHILE }
    | "do"	{ DO }
    | "func"    { FUNC }
    | "stream"  { STREAM }
    | "null"    { NULL }
    | "undefined" { UNDEFINED }

    (* variable types *)
    | "int"  | "float" | "bool" as lxm { TYPE(type_of_string lxm) }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm) }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as lxm { VARNAME(lxm) }

    | eof      { raise Eof }
