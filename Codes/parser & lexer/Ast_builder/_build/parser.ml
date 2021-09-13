
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | Smaller
    | STAR
    | SRPAR
    | SLPAR
    | SLASH
    | SEMICOLON
    | RPAR
    | PRIME
    | PLUS
    | OR
    | Num of (
# 7 "parser.mly"
       (string)
# 21 "parser.ml"
  )
    | MINUS
    | Larger
    | LPAR
    | IN
    | IDENTIFIER of (
# 6 "parser.mly"
       (string)
# 30 "parser.ml"
  )
    | FORALL
    | EXISTS
    | EQUAL
    | EOF
    | COMMA
    | COLON
    | ASSIGN
    | ARROW
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState113
  | MenhirState111
  | MenhirState106
  | MenhirState101
  | MenhirState98
  | MenhirState92
  | MenhirState90
  | MenhirState85
  | MenhirState83
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState51
  | MenhirState47
  | MenhirState45
  | MenhirState42
  | MenhirState38
  | MenhirState33
  | MenhirState31
  | MenhirState26
  | MenhirState24
  | MenhirState20
  | MenhirState17
  | MenhirState15
  | MenhirState13
  | MenhirState8
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 1 "parser.mly"
   
# 95 "parser.ml"

let rec _menhir_goto_tla_file : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.tla_fil) -> 'ttv_return =
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
            let _menhir_s = MenhirState111 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.tla_fil))) = _menhir_stack in
            let _v : (Ast.tla_fil) = 
# 17 "parser.mly"
                           ( _1 )
# 114 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.tla_fil)) = _v in
            Obj.magic _1
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState113 | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.tla_fil))), _, (_2 : (Ast.tla_fil))) = _menhir_stack in
            let _v : (Ast.tla_fil) = 
# 21 "parser.mly"
                     (Ast.MulDef (_1, _2))
# 139 "parser.ml"
             in
            _menhir_goto_tla_file _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        _menhir_fail ()

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.temp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FORALL ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENTIFIER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.temp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FORALL ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENTIFIER _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_goto_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.definition) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.definition)) = _v in
    let _v : (Ast.tla_fil) = 
# 20 "parser.mly"
               (Ast.Definition (_1) )
# 201 "parser.ml"
     in
    _menhir_goto_tla_file _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_goto_primed_eq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.temp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.temp)) = _v in
    let _v : (Ast.temp) = 
# 37 "parser.mly"
            ( _1 )
# 267 "parser.ml"
     in
    _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_goto_temporal_formula : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.temp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 327 "parser.ml"
            ))), _, (_2 : (string list))), _, (_4 : (Ast.temp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 32 "parser.mly"
                   (_2)
# 332 "parser.ml"
             in
            
# 25 "parser.mly"
                                                                           (Ast.Statment (Ast.DEFIN ( _1), _2 , Ast.ASSIG , Ast.Stat _4, Ast.NEWL ) )
# 337 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.temp))), _, (_3 : (Ast.temp))) = _menhir_stack in
            let _v : (Ast.temp) = 
# 38 "parser.mly"
                                          (Ast.Mix (_1,Ast.Disjun,_3))
# 361 "parser.ml"
             in
            _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.temp))), _, (_3 : (Ast.temp))) = _menhir_stack in
            let _v : (Ast.temp) = 
# 39 "parser.mly"
                                            (Ast.Mix (_1,Ast.Conj,_3) )
# 385 "parser.ml"
             in
            _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 410 "parser.ml"
            ))), _, (_2 : (string list))), _, (_4 : (Ast.temp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 31 "parser.mly"
                 ( _2)
# 415 "parser.ml"
             in
            
# 25 "parser.mly"
                                                                           (Ast.Statment (Ast.DEFIN ( _1), _2 , Ast.ASSIG , Ast.Stat _4, Ast.NEWL ) )
# 420 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 445 "parser.ml"
            ))), _, (_4 : (Ast.temp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 30 "parser.mly"
  ([])
# 450 "parser.ml"
             in
            
# 25 "parser.mly"
                                                                           (Ast.Statment (Ast.DEFIN ( _1), _2 , Ast.ASSIG , Ast.Stat _4, Ast.NEWL ) )
# 455 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.pred) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FORALL ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENTIFIER _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.pred) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FORALL ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENTIFIER _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 88 "parser.mly"
                       ( _2 )
# 532 "parser.ml"
             in
            _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
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
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SRPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 557 "parser.ml"
            ))), (_4 : (
# 6 "parser.mly"
       (string)
# 561 "parser.ml"
            ))), _, (_6 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 89 "parser.mly"
                                                     (Ast.Func_def (Ast.Var(_2),Ast.Var _4 ,_6) )
# 566 "parser.ml"
             in
            _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
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
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 589 "parser.ml"
            ))), _, (_2 : (string list))), _, (_5 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.temp) = let _2 = 
# 32 "parser.mly"
                   (_2)
# 594 "parser.ml"
             in
            
# 43 "parser.mly"
                                                          ( Ast.Prime ( Ast.Var _1, _2 , _5 ) )
# 599 "parser.ml"
             in
            _menhir_goto_primed_eq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 622 "parser.ml"
            ))), _, (_5 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.temp) = let _2 = 
# 30 "parser.mly"
  ([])
# 627 "parser.ml"
             in
            
# 43 "parser.mly"
                                                          ( Ast.Prime ( Ast.Var _1, _2 , _5 ) )
# 632 "parser.ml"
             in
            _menhir_goto_primed_eq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 655 "parser.ml"
            ))), _, (_2 : (string list))), _, (_5 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.temp) = let _2 = 
# 31 "parser.mly"
                 ( _2)
# 660 "parser.ml"
             in
            
# 43 "parser.mly"
                                                          ( Ast.Prime ( Ast.Var _1, _2 , _5 ) )
# 665 "parser.ml"
             in
            _menhir_goto_primed_eq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 | MenhirState92 | MenhirState57 | MenhirState69 | MenhirState83 | MenhirState85 | MenhirState71 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | Larger ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | Smaller ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.prop) = 
# 59 "parser.mly"
                       ( Ast.Inequality (_1,Ast.Less,_3) )
# 710 "parser.ml"
             in
            _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.prop) = 
# 58 "parser.mly"
                       ( Ast.Inequality (_1,Ast.Greater,_3) )
# 734 "parser.ml"
             in
            _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | OR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.prop) = 
# 57 "parser.mly"
                       ( Ast.Equality (_1,Ast.EQ,_3) )
# 758 "parser.ml"
             in
            _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | Larger ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 787 "parser.ml"
            ))), _, (_2 : (string list))), _, (_4 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 32 "parser.mly"
                   (_2)
# 792 "parser.ml"
             in
            
# 26 "parser.mly"
                                                                (Ast.Value (Ast.DEFIN ( _1) ,_2,Ast.ASSIG , Ast.Expr _4 , Ast.NEWL ) )
# 797 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | Smaller ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | Larger ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 828 "parser.ml"
            ))), _, (_2 : (string list))), _, (_4 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 31 "parser.mly"
                 ( _2)
# 833 "parser.ml"
             in
            
# 26 "parser.mly"
                                                                (Ast.Value (Ast.DEFIN ( _1) ,_2,Ast.ASSIG , Ast.Expr _4 , Ast.NEWL ) )
# 838 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | Smaller ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | Larger ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 869 "parser.ml"
            ))), _, (_4 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.definition) = let _2 = 
# 30 "parser.mly"
  ([])
# 874 "parser.ml"
             in
            
# 26 "parser.mly"
                                                                (Ast.Value (Ast.DEFIN ( _1) ,_2,Ast.ASSIG , Ast.Expr _4 , Ast.NEWL ) )
# 879 "parser.ml"
             in
            _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v
        | Smaller ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_goto_predicate : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pred) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 947 "parser.ml"
            ))), (_4 : (
# 6 "parser.mly"
       (string)
# 951 "parser.ml"
            ))), _, (_6 : (Ast.pred))) = _menhir_stack in
            let _v : (Ast.pred) = 
# 49 "parser.mly"
                                                     ( Ast.Existence (Ast.Exis,Ast.Var _2, Ast.Inclus, Ast.Var _4, Ast.Col, _6 ) )
# 956 "parser.ml"
             in
            _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.pred))), _, (_3 : (Ast.pred))) = _menhir_stack in
            let _v : (Ast.pred) = 
# 51 "parser.mly"
                          ( Ast.Pred_Comp (_1,Ast.Disjun,_3)  )
# 980 "parser.ml"
             in
            _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.pred))), _, (_3 : (Ast.pred))) = _menhir_stack in
            let _v : (Ast.pred) = 
# 52 "parser.mly"
                           ( Ast.Pred_Comp (_1,Ast.Conj,_3)  )
# 1004 "parser.ml"
             in
            _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (_2 : (
# 6 "parser.mly"
       (string)
# 1027 "parser.ml"
            ))), (_4 : (
# 6 "parser.mly"
       (string)
# 1031 "parser.ml"
            ))), _, (_6 : (Ast.pred))) = _menhir_stack in
            let _v : (Ast.pred) = 
# 50 "parser.mly"
                                                     ( Ast.Universal (Ast.Univ,Ast.Var _2, Ast.Inclus, Ast.Var _4, Ast.Col, _6 ) )
# 1036 "parser.ml"
             in
            _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 | MenhirState101 | MenhirState8 | MenhirState90 | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.pred))) = _menhir_stack in
            let _v : (Ast.temp) = 
# 36 "parser.mly"
            ( Ast.Predec _1)
# 1060 "parser.ml"
             in
            _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.prop) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.prop) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_reduce28 : _menhir_env -> ((('ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1111 "parser.ml"
))) * _menhir_state * (string list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1117 "parser.ml"
    ))), _, (_2 : (string list))) = _menhir_stack in
    let _v : (Ast.prop) = let _2 = 
# 31 "parser.mly"
                 ( _2)
# 1122 "parser.ml"
     in
    
# 60 "parser.mly"
                                               (Ast.Open_prop (Ast.DEFIN(_1),_2) )
# 1127 "parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce29 : _menhir_env -> ((('ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1134 "parser.ml"
))) * _menhir_state * (string list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1140 "parser.ml"
    ))), _, (_2 : (string list))) = _menhir_stack in
    let _v : (Ast.prop) = let _2 = 
# 32 "parser.mly"
                   (_2)
# 1145 "parser.ml"
     in
    
# 60 "parser.mly"
                                               (Ast.Open_prop (Ast.DEFIN(_1),_2) )
# 1150 "parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> ((('ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1157 "parser.ml"
))) * _menhir_state * (string list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1163 "parser.ml"
    ))), _, (_2 : (string list))) = _menhir_stack in
    let _v : (Ast.exp) = let _2 = 
# 31 "parser.mly"
                 ( _2)
# 1168 "parser.ml"
     in
    
# 86 "parser.mly"
                                              (Ast.Var (_1) )
# 1173 "parser.ml"
     in
    _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> ((('ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1180 "parser.ml"
))) * _menhir_state * (string list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1186 "parser.ml"
    ))), _, (_2 : (string list))) = _menhir_stack in
    let _v : (Ast.exp) = let _2 = 
# 32 "parser.mly"
                   (_2)
# 1191 "parser.ml"
     in
    
# 86 "parser.mly"
                                              (Ast.Var (_1) )
# 1196 "parser.ml"
     in
    _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState106 | MenhirState101 | MenhirState92 | MenhirState90 | MenhirState8 | MenhirState57 | MenhirState85 | MenhirState83 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState51 | MenhirState45 | MenhirState42 | MenhirState13 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | Larger | MINUS | OR | PLUS | RPAR | SEMICOLON | SRPAR | Smaller ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 76 "parser.mly"
                       ( _1 )
# 1224 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | Larger | MINUS | OR | PLUS | RPAR | SEMICOLON | SRPAR | Smaller ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 74 "parser.mly"
                       ( Ast.Binop (_1,Ast.Add,_3) )
# 1248 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | AND | EQUAL | Larger | MINUS | OR | PLUS | RPAR | SEMICOLON | SRPAR | Smaller ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 75 "parser.mly"
                       ( Ast.Binop (_1,Ast.Sub,_3) )
# 1272 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_proposition : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.prop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState106 | MenhirState101 | MenhirState8 | MenhirState90 | MenhirState92 | MenhirState57 | MenhirState85 | MenhirState83 | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.prop))) = _menhir_stack in
            let _v : (Ast.pred) = 
# 48 "parser.mly"
              (Ast.Prop _1 )
# 1303 "parser.ml"
             in
            _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.prop))), _, (_3 : (Ast.prop))) = _menhir_stack in
            let _v : (Ast.prop) = 
# 62 "parser.mly"
                              ( Ast.Coposition (_1,Ast.Disjun,_3)  )
# 1327 "parser.ml"
             in
            _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.prop))), _, (_3 : (Ast.prop))) = _menhir_stack in
            let _v : (Ast.prop) = 
# 61 "parser.mly"
                               ( Ast.Coposition (_1,Ast.Conj,_3)  )
# 1351 "parser.ml"
             in
            _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_varlist : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1373 "parser.ml"
        ))), _, (_3 : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 68 "parser.mly"
                                     (_1::_3)
# 1378 "parser.ml"
         in
        _menhir_goto_varlist _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SRPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXISTS ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                | FORALL ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                | IDENTIFIER _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
                | LPAR ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                | Num _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
                | SLPAR ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SRPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
        | SRPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PRIME ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQUAL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENTIFIER _v ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
                    | LPAR ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | Num _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
                    | SLPAR ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | OR | SEMICOLON ->
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
            | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
                _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PRIME ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQUAL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENTIFIER _v ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | LPAR ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | Num _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | SLPAR ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | AND | OR | SEMICOLON ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
            | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
                _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SRPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AND | OR | SEMICOLON ->
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
            | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
                _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AND | OR | SEMICOLON ->
                _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
            | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
                _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXISTS ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | FORALL ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | IDENTIFIER _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | LPAR ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | Num _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | SLPAR ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Ast.exp)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 80 "parser.mly"
                       ( Ast.Binop(_1,Ast.Mul,_3) )
# 1671 "parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Ast.exp)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 81 "parser.mly"
                       ( Ast.Binop(_1,Ast.Div,_3) )
# 1682 "parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 | MenhirState101 | MenhirState8 | MenhirState90 | MenhirState92 | MenhirState57 | MenhirState69 | MenhirState83 | MenhirState85 | MenhirState71 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState73 | MenhirState51 | MenhirState45 | MenhirState42 | MenhirState13 | MenhirState33 | MenhirState31 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.exp)) = _v in
        let _v : (Ast.exp) = 
# 82 "parser.mly"
                       ( _1 )
# 1692 "parser.ml"
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1701 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1707 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.exp) = let _2 = 
# 30 "parser.mly"
  ([])
# 1712 "parser.ml"
     in
    
# 86 "parser.mly"
                                              (Ast.Var (_1) )
# 1717 "parser.ml"
     in
    _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 6 "parser.mly"
       (string)
# 1724 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1730 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.prop) = let _2 = 
# 30 "parser.mly"
  ([])
# 1735 "parser.ml"
     in
    
# 60 "parser.mly"
                                               (Ast.Open_prop (Ast.DEFIN(_1),_2) )
# 1740 "parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1747 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | RPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | SLPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | SRPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | AND | EQUAL | Larger | MINUS | OR | PLUS | RPAR | SEMICOLON | SLASH | SRPAR | STAR | Smaller ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1792 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | RPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | SLPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | SRPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | AND | OR | SEMICOLON ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 66 "parser.mly"
         ([])
# 1841 "parser.ml"
     in
    _menhir_goto_varlist _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1848 "parser.ml"
) -> 'ttv_return =
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
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | RPAR | SRPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
    | RPAR | SRPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 6 "parser.mly"
       (string)
# 1873 "parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 67 "parser.mly"
                      ([_1])
# 1878 "parser.ml"
         in
        _menhir_goto_varlist _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ARROW ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENTIFIER _v ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                    | LPAR ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | Num _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                    | SLPAR ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 1956 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 7 "parser.mly"
       (string)
# 1964 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 87 "parser.mly"
                        (Ast.INT _1)
# 1969 "parser.ml"
     in
    _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAR ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | Num _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | SLPAR ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 1995 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | RPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | PRIME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LPAR ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | Num _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | SLPAR ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SLPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | SRPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | AND | OR | SEMICOLON ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | EQUAL | Larger | MINUS | PLUS | SLASH | STAR | Smaller ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EXISTS ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | FORALL ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | IDENTIFIER _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | LPAR ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | Num _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                    | SLPAR ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EXISTS ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                    | FORALL ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                    | IDENTIFIER _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
                    | LPAR ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                    | Num _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
                    | SLPAR ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
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
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (string)
# 2351 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EXISTS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | FORALL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IDENTIFIER _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | LPAR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | Num _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | SLPAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | RPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | SLPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | SRPAR ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.tla_fil) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2446 "parser.ml"
