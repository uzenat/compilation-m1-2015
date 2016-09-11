
(* The type of tokens. *)

type token = 
  | UPPERSAND
  | SUB
  | SEMICOLON
  | RPAREN
  | RID of (string)
  | RET
  | RARROW
  | MUL
  | LTE
  | LT
  | LPAREN
  | LOCAL
  | LOAD
  | LARROW
  | JUMPIF
  | JUMP
  | INT of (Int32.t)
  | ID of (string)
  | GTE
  | GT
  | EXTERNAL
  | EXIT
  | EQ
  | EOF
  | END
  | DIV
  | DEF
  | COMMENT of (string)
  | COMMA
  | COLON
  | CODE
  | CALL
  | BSET
  | BGET
  | BCREATE
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (RetrolixAST.t)
