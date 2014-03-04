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

    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | "func"    { FUNC }
    | "stream"  { STREAM }

    | ":"       { COLON }
    | "<<"      { LCHEVRONS }
    | "~"       { TILDE }

    | "="       { ASSIGN }
    | ":="      { TYPE_ASSIGN }

    | '('       { LPAREN }
    | ')'       { RPAREN }

    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIV }
    | '^'       { EXPONENTIAL }
    | '%'       { MODULO }

    | ','       { COMMA }

    | "true"    { TRUE }
    | "false"   { FALSE }

    | "int" | "string" | "float" | "bool" as lxm { TYPE(type_of_string lxm) }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as lxm { VARNAME(lxm) }
    | eof      { raise Eof }
