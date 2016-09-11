
exception Error

let _eRR =
  Error

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

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState93
  | MenhirState88
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState71
  | MenhirState68
  | MenhirState63
  | MenhirState60
  | MenhirState54
  | MenhirState52
  | MenhirState49
  | MenhirState47
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState10
  | MenhirState7
  | MenhirState4
  | MenhirState0
  

  open FopixAST


let rec _menhir_goto_separated_nonempty_list_PIPE_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (FopixAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_PIPE_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _2 = () in
        let _v : (FopixAST.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_PIPE_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_PIPE_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, e), _, xs0) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (FopixAST.expression) = let bs =
          let xs = xs0 in
              ( xs )
        in
        (
  Switch (e, Array.of_list bs, None)
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | ORELSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SWITCH ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | UPPERSAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | VAL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_located_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (FopixAST.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_located_expression___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x0) = _menhir_stack in
        let _2 = () in
        let _v : (FopixAST.expression list) = let x =
          let x = x0 in
                                  (
  x
)
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_located_expression__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x0) = _menhir_stack in
            let _v : (FopixAST.expression list) = let x =
              let x = x0 in
                                      (
  x
)
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_located_expression__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LAND | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SLASH | STAR | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`*"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGNS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
                | IF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
                | INT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
                | LPAREN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
                | SWITCH ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
                | UPPERSAND ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
                | VAL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
            | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LAND | LBRACKET | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SLASH | STAR | THEN | VAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (FopixAST.expression) = let i =
                  let x = x1 in
                                          (
  x
)
                in
                let e =
                  let x = x0 in
                                          (
  x
)
                in
                                                                                (
  FunCall (FunId "block_get", [e; i])
) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LAND | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SLASH | STAR | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`/"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _2 = () in
            let _v : (FopixAST.expression) = let e2 =
              let x = x1 in
                                      (
  x
)
            in
            let e1 =
              let x = x0 in
                                      (
  x
)
            in
                                                                      (
  Define (Id "_", e1, e2)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`+"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LAND | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SLASH | STAR | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`&&" )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`-"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`<=" )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`||" )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`<"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`>=" )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`>"  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x0), _, x1) = _menhir_stack in
            let _10 = () in
            let _v : (FopixAST.expression) = let r =
              let x = x1 in
                                      (
  x
)
            in
            let b =
              let _1 = _10 in
                      ( "`="  )
            in
            let l =
              let x = x0 in
                                      (
  x
)
            in
                                                                  (
  FunCall (FunId b, [l; r])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DEF | ELSE | END | EOF | EVAL | EXTERNAL | IN | ORELSE | PIPE | RBRACKET | RPAREN | SEMICOLON | THEN | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x0), _, x1), _, x2) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (FopixAST.expression) = let v =
              let x = x2 in
                                      (
  x
)
            in
            let i =
              let x = x1 in
                                      (
  x
)
            in
            let e =
              let x = x0 in
                                      (
  x
)
            in
                                            (
  FunCall (FunId "block_set", [e; i; v])
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, x0), _, x1), _, x2) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.expression) = let f =
              let x = x2 in
                                      (
  x
)
            in
            let t =
              let x = x1 in
                                      (
  x
)
            in
            let c =
              let x = x0 in
                                      (
  x
)
            in
            (
  IfThenElse (c, t, f)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.expression) =                              (
  e
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | END | ORELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState60 in
                let _v : (FopixAST.expression list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_PIPE_expression__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, e), _, xs0), _, d) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.expression) = let bs =
              let xs = xs0 in
                  ( xs )
            in
            (
  Switch (e, Array.of_list bs, Some d)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | END | ORELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (FopixAST.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_PIPE_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), x00), _, x0), _, x1) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.expression) = let e2 =
              let x = x1 in
                                      (
  x
)
            in
            let e1 =
              let x = x0 in
                                      (
  x
)
            in
            let x =
              let x0 = x00 in
              let x =
                let x = x0 in
                                         (
  Id x
)
              in
                                      (
  x
)
            in
            (
  Define (x, e1, e2)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DEF | EOF | EVAL | EXTERNAL | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), x00), _, x0) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.definition) = let e =
              let x = x0 in
                                      (
  x
)
            in
            let x =
              let x0 = x00 in
              let x =
                let x = x0 in
                                         (
  Id x
)
              in
                                      (
  x
)
            in
            (
  DefineValue (x, e)
) in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DEF | EOF | EVAL | EXTERNAL | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
            let _1 = () in
            let _v : (FopixAST.definition) = let e =
              let x = x0 in
                                      (
  x
)
            in
            (
  DefineValue (Id "_", e)
) in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GTE ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LTE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DEF | EOF | EVAL | EXTERNAL | VAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), x00), _, xs0), _, x0) = _menhir_stack in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (FopixAST.definition) = let e =
              let x = x0 in
                                      (
  x
)
            in
            let xs =
              let xs = xs0 in
                  ( xs )
            in
            let f =
              let x0 = x00 in
              let x =
                let x = x0 in
                                                  (
  FunId x
)
              in
                                      (
  x
)
            in
            (
  DefineFunction (f, xs, e)
) in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_located_expression___ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x0), _, xs0) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (FopixAST.expression) = let es =
          let xs = xs0 in
              ( xs )
        in
        let f =
          let x = x0 in
                                            (
  FunId x
)
        in
        (
  FunCall (f, es)
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_identifier_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.formals) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x0) = _menhir_stack in
        let _2 = () in
        let _v : (FopixAST.formals) = let x =
          let x = x0 in
                                   (
  Id x
)
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_identifier_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (FopixAST.formals) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_identifier__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_list_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, ds) = _menhir_stack in
            let _2 = () in
            let _v : (FopixAST.t) = (
  ds
) in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (FopixAST.t) =     ( x :: xs ) in
        _menhir_goto_list_definition_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.definition) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EVAL ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EXTERNAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VAL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EOF ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x00 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _10 = () in
        let _v : (FopixAST.expression) = let l =
          let x0 = x00 in
          let _1 = _10 in
          let f =
            let x = x0 in
                                              (
  FunId x
)
          in
          (
  LFun f
)
        in
        (
  Literal l
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Int32.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x0 = _v in
    let _v : (FopixAST.expression) = let l =
      let x = x0 in
      (
  LInt x
)
    in
    (
  Literal l
) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | IF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | INT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | LPAREN ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | SWITCH ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | UPPERSAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | VAL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState15 in
            let _v : (FopixAST.expression list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_located_expression___ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | COMMA | DEF | ELSE | END | EOF | EQUAL | EVAL | EXTERNAL | GT | GTE | IN | LAND | LBRACKET | LOR | LT | LTE | MINUS | ORELSE | PIPE | PLUS | RBRACKET | RPAREN | SEMICOLON | SLASH | STAR | THEN | VAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x0) = _menhir_stack in
        let _v : (FopixAST.expression) = let x =
          let x = x0 in
                                   (
  Id x
)
        in
        (
  Variable x
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_identifier__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (FopixAST.formals) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x0) = _menhir_stack in
        let _v : (FopixAST.formals) = let x =
          let x = x0 in
                                   (
  Id x
)
        in
            ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_identifier_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__1_ = _endpos in
        let _startpos__1_ = _startpos in
        let _1 = () in
        let _v : (FopixAST.t) = let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
                (
  let pos = Position.lex_join _startpos _endpos in
  Error.error "parsing" pos "Syntax error."
) in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (FopixAST.t) =     ( [] ) in
    _menhir_goto_list_definition_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | IF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | INT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | SWITCH ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | UPPERSAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | VAL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x00 = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (FopixAST.definition) = let f =
          let x0 = x00 in
          let x =
            let x = x0 in
                                              (
  FunId x
)
          in
                                  (
  x
)
        in
        (
  ExternalFunction f
) in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SWITCH ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | UPPERSAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | VAL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState81 in
                let _v : (FopixAST.formals) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_identifier__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (FopixAST.t) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EVAL ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EXTERNAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAL ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

