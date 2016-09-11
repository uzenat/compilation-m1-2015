
(* The type of tokens. *)

type token = 
  | VAL
  | USCORE
  | TYPE
  | TVAR of (string)
  | THEN
  | STRING of (string)
  | STAR
  | SLASH
  | SHARP
  | SEMICOLON
  | RPAREN
  | REC
  | RCURLY
  | RBRACK
  | RARROW
  | QMARK
  | PREFIXID of (string)
  | PLUS
  | PIPE
  | OR
  | MINUS
  | LT
  | LPAREN
  | LEQ
  | LCURLY
  | LBRACK
  | LARROW
  | KID of (string)
  | INT of (Int32.t)
  | INFIXID of (string)
  | IF
  | GT
  | GEQ
  | FI
  | FATARROW
  | EXTERN
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DONE
  | DO
  | DEQUAL
  | COMMA
  | COLON
  | CHAR of (char)
  | BSLASH
  | BOOL of (bool)
  | BID of (string)
  | AND
  | AMPER
  | AAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HopixAST.t)
