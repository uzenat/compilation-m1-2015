
(* The type of tokens. *)

type token = 
  | VAL
  | UPPERSAND
  | THEN
  | SWITCH
  | STAR
  | SLASH
  | SEMICOLON
  | RPAREN
  | RBRACKET
  | PLUS
  | PIPE
  | ORELSE
  | MINUS
  | LTE
  | LT
  | LPAREN
  | LOR
  | LBRACKET
  | LAND
  | INT of (Int32.t)
  | IN
  | IF
  | ID of (string)
  | GTE
  | GT
  | EXTERNAL
  | EVAL
  | EQUAL
  | EOF
  | END
  | ELSE
  | DEF
  | COMMA
  | ASSIGNS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (FopixAST.t)
