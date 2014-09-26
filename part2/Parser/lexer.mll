{
open Parser
exception Eof

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let ident = (['a'-'z'] | '_') (['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_' | '\'')*
let integral = ['0'-'9']+
let whitespace = [' ' '\t']
let fname = (['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'] | '.')* ".ml"

rule token = parse
  | whitespace { token lexbuf } (* skip blanks *)
  | ['\n'] { incr_linenum lexbuf; token lexbuf }
  | "[" { LBRACE }
  | ";" { SEMICOLON }
  | "]" { RBRACE }
  | "::" { CONS }
  | "fun"  { FUN }
  | "false" { FALSE }
  | "true" { TRUE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { PIPE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIVIDE }
  | "mod" { MOD }
  | ">" { GT }
  | "<" { LT }
  | "<=" { LTQ }
  | ">=" { GTQ }
  | "=" { EQ }
  | "<>" { NEQ }
  | "not" { NOT }
  | "rec" { REC }
  | "let" { LET }
  | "in" { IN } 
  | "&&" { AND }
  | "||" { OR }
  | "()" { UNIT }
  | '(' | "begin"    { LPAREN }
  | ')' | "end"   { RPAREN }
  | "->"   { IMP }
  | "#use"   | "#u" { USE }
  | "#reset" | "#r" { RESET }
  | "#quit"  | "#q" { QUIT }
  | "#help"  | "#h" { HELP }
  | "#env"   | "#e" { ENV }
  | "ref" { REF }
  | ":=" { ASSIGN }
  | "!" { DEREF }
  | ident as id { VAR id }
  | integral as i {INT i}
  | fname as f { FILE f }
  | eof { EOF }
