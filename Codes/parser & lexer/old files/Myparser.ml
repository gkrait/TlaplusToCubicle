
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TWO_PERIODS
    | TRUE
    | STR
    | RSQUARE_BRACKET
    | RCURLY_BRACKET
    | PRIME
    | PLUS
    | OR
    | OPEN
    | Num
    | NOTEQUAL
    | NOT
    | MINUS
    | LSQUARE_BRACKET
    | LESS
    | LCURLY_BRACKET
    | LARGER
    | IN
    | IDEN1 of (
# 28 "Myparser.mly"
      (string)
# 29 "Myparser.ml"
  )
    | FORALL
    | FALSE
    | EXISTS
    | EQUAL
    | EOL
    | COMA
    | COLON
    | CLOSE
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
  | MenhirState85
  | MenhirState84
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState61
  | MenhirState57
  | MenhirState56
  | MenhirState52
  | MenhirState48
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState35
  | MenhirState34
  | MenhirState30
  | MenhirState26
  | MenhirState24
  | MenhirState23
  | MenhirState19
  | MenhirState17
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState8
  | MenhirState6
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 2 "Myparser.mly"
  



# 97 "Myparser.ml"

let rec _menhir_goto_list_coma_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_coma_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv319 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv317 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (va : 'tv_value)), _, (xs : 'tv_list_coma_value_)) = _menhir_stack in
        let _v : 'tv_list_coma_value_ = let x = 
# 103 "Myparser.mly"
                   (va)
# 112 "Myparser.ml"
         in
        
# 213 "<standard.mly>"
    ( x :: xs )
# 117 "Myparser.ml"
         in
        _menhir_goto_list_coma_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)) : 'freshtv320)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv327 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RCURLY_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv323 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv321 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (v : 'tv_value)), _, _) = _menhir_stack in
            let _v : 'tv_finite_set = 
# 99 "Myparser.mly"
                                                            (v)
# 136 "Myparser.ml"
             in
            _menhir_goto_finite_set _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state) * _menhir_state * 'tv_value) * _menhir_state * 'tv_list_coma_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)
    | _ ->
        _menhir_fail ()

and _menhir_goto_predicate : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_predicate -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv307 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_predicate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv305 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((p : 'tv_predicate) : 'tv_predicate) = _v in
        ((let (((_menhir_stack, _menhir_s), _, _), _, _) = _menhir_stack in
        let e = () in
        let _v : 'tv_predicate = let q = 
# 63 "Myparser.mly"
           (e)
# 166 "Myparser.ml"
         in
        
# 57 "Myparser.mly"
                                                              (q::p)
# 171 "Myparser.ml"
         in
        _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)) : 'freshtv308)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv311 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_predicate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv309 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((p : 'tv_predicate) : 'tv_predicate) = _v in
        ((let (((_menhir_stack, _menhir_s), _, _), _, _) = _menhir_stack in
        let f = () in
        let _v : 'tv_predicate = let q = 
# 64 "Myparser.mly"
           (f)
# 188 "Myparser.ml"
         in
        
# 57 "Myparser.mly"
                                                              (q::p)
# 193 "Myparser.ml"
         in
        _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_predicate) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv313) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((p : 'tv_predicate) : 'tv_predicate) = _v in
        ((let _v : 'tv_temporal_formula = 
# 51 "Myparser.mly"
              (p)
# 208 "Myparser.ml"
         in
        _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
    | _ ->
        _menhir_fail ()

and _menhir_goto_logical_junction : _menhir_env -> 'ttv_tail -> 'tv_logical_junction -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv303 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv304)

and _menhir_goto_temporal_formula : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_temporal_formula -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_temporal_formula) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_temporal_formula) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_temporal_formula) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (tf : 'tv_temporal_formula)) = _menhir_stack in
        let _v : (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 262 "Myparser.ml"
        ) = 
# 46 "Myparser.mly"
                            (tf)
# 266 "Myparser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 274 "Myparser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 282 "Myparser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 290 "Myparser.ml"
        )) : (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 294 "Myparser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv290)) : 'freshtv292)) : 'freshtv294)) : 'freshtv296)) : 'freshtv298)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_temporal_formula) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_coma_value_ = 
# 211 "<standard.mly>"
    ( [] )
# 310 "Myparser.ml"
     in
    _menhir_goto_list_coma_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_goto_propositional_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_propositional_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (p : 'tv_propositional_exp)) = _menhir_stack in
        let _v : 'tv_propositional_exp = 
# 71 "Myparser.mly"
                          (p)
# 352 "Myparser.ml"
         in
        _menhir_goto_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv275 * _menhir_state) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265) = Obj.magic _menhir_stack in
            ((let a = () in
            let _v : 'tv_logical_junction = 
# 76 "Myparser.mly"
        ()
# 371 "Myparser.ml"
             in
            _menhir_goto_logical_junction _menhir_env _menhir_stack _v) : 'freshtv266)) : 'freshtv268)
        | OR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269) = Obj.magic _menhir_stack in
            ((let o = () in
            let _v : 'tv_logical_junction = 
# 77 "Myparser.mly"
        ()
# 384 "Myparser.ml"
             in
            _menhir_goto_logical_junction _menhir_env _menhir_stack _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv273 * _menhir_state) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv283 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLOSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv279 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv277 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (pr1 : 'tv_propositional_exp)), (l : 'tv_logical_junction)), _, (pr2 : 'tv_propositional_exp)) = _menhir_stack in
            let _v : 'tv_propositional_exp = 
# 72 "Myparser.mly"
                                                                                (pr1 l pr2)
# 410 "Myparser.ml"
             in
            _menhir_goto_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv281 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | MenhirState0 | MenhirState68 | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state * 'tv_propositional_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (p : 'tv_propositional_exp)) = _menhir_stack in
        let _v : 'tv_predicate = 
# 56 "Myparser.mly"
                      (p)
# 429 "Myparser.ml"
         in
        _menhir_goto_predicate _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | _ ->
        _menhir_fail ()

and _menhir_goto_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_operator -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv259 * _menhir_state * 'tv_value) * _menhir_state * 'tv_operator) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv260)

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_value -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_value -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_goto_simple_propositional_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_propositional_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_simple_propositional_exp) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((si : 'tv_simple_propositional_exp) : 'tv_simple_propositional_exp) = _v in
    ((let _v : 'tv_propositional_exp = 
# 69 "Myparser.mly"
                              (si)
# 525 "Myparser.ml"
     in
    _menhir_goto_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)

and _menhir_goto_nonempty_list_numeral_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_numeral_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state * 'tv_numeral) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_numeral) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_numeral)), _, (xs : 'tv_nonempty_list_numeral_)) = _menhir_stack in
        let _v : 'tv_nonempty_list_numeral_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 542 "Myparser.ml"
         in
        _menhir_goto_nonempty_list_numeral_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)) : 'freshtv236)
    | MenhirState84 | MenhirState0 | MenhirState68 | MenhirState73 | MenhirState3 | MenhirState61 | MenhirState56 | MenhirState40 | MenhirState42 | MenhirState5 | MenhirState34 | MenhirState13 | MenhirState23 | MenhirState26 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (n : 'tv_nonempty_list_numeral_)) = _menhir_stack in
        let _v : 'tv_value = 
# 107 "Myparser.mly"
                           (n)
# 554 "Myparser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TWO_PERIODS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | Num ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_nonempty_list_numeral_)) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_nonempty_list_numeral_)) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (n1 : 'tv_nonempty_list_numeral_)), _, (n2 : 'tv_nonempty_list_numeral_)) = _menhir_stack in
        let _v : 'tv_finite_set = 
# 95 "Myparser.mly"
                                                                  (n1::n2)
# 593 "Myparser.ml"
         in
        _menhir_goto_finite_set _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)) : 'freshtv250)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_nonempty_list_numeral_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_identifier)), _, (n1 : 'tv_nonempty_list_numeral_)) = _menhir_stack in
        let _v : 'tv_finite_set = 
# 97 "Myparser.mly"
                                        (i::n1)
# 605 "Myparser.ml"
         in
        _menhir_goto_finite_set _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_value -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv181 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | RSQUARE_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv179 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState15 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv177 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (i1 : 'tv_identifier)), _, (i2 : 'tv_identifier)), _, (v : 'tv_value)) = _menhir_stack in
            let _v : 'tv_func_tla = 
# 133 "Myparser.mly"
                                                                                    (v)
# 637 "Myparser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv175) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_tla) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_func_tla) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((f : 'tv_func_tla) : 'tv_func_tla) = _v in
            ((let _v : 'tv_value = 
# 110 "Myparser.mly"
             (f)
# 654 "Myparser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv182)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv185 * _menhir_state * 'tv_value) * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv183 * _menhir_state * 'tv_value) * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (v1 : 'tv_value)), _), _, (v : 'tv_value)) = _menhir_stack in
        let p = () in
        let _v : 'tv_value = let _2 = 
# 117 "Myparser.mly"
         (p)
# 671 "Myparser.ml"
         in
        
# 113 "Myparser.mly"
                         (v)
# 676 "Myparser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | RSQUARE_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState24 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, (i : 'tv_identifier)), _, (v : 'tv_value)) = _menhir_stack in
            let _v : 'tv_value = 
# 111 "Myparser.mly"
                                                       (i::v)
# 701 "Myparser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv192)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv195 * _menhir_state * 'tv_value) * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv193 * _menhir_state * 'tv_value) * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (v1 : 'tv_value)), _), _, (v : 'tv_value)) = _menhir_stack in
        let m = () in
        let _v : 'tv_value = let _2 = 
# 118 "Myparser.mly"
          (m)
# 718 "Myparser.ml"
         in
        
# 113 "Myparser.mly"
                         (v)
# 723 "Myparser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)
    | MenhirState0 | MenhirState68 | MenhirState73 | MenhirState61 | MenhirState3 | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LARGER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState30 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            ((let la = () in
            let _v : 'tv_operator = 
# 83 "Myparser.mly"
            (la)
# 744 "Myparser.ml"
             in
            _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)
        | LESS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState30 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            ((let le = () in
            let _v : 'tv_operator = 
# 82 "Myparser.mly"
           (le)
# 759 "Myparser.ml"
             in
            _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)) : 'freshtv204)
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NOTEQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState30 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            ((let ne = () in
            let _v : 'tv_operator = 
# 81 "Myparser.mly"
              (ne)
# 776 "Myparser.ml"
             in
            _menhir_goto_operator _menhir_env _menhir_stack _menhir_s _v) : 'freshtv206)) : 'freshtv208)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv210)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_value) * _menhir_state * 'tv_operator) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | AND | CLOSE | EOL | OR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_value) * _menhir_state * 'tv_operator) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (v1 : 'tv_value)), _, (op : 'tv_operator)), _, (v2 : 'tv_value)) = _menhir_stack in
            let _v : 'tv_propositional_exp = 
# 70 "Myparser.mly"
                                (v1 op v2)
# 802 "Myparser.ml"
             in
            _menhir_goto_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv214)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMA ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | RCURLY_BRACKET ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv216)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMA ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | RCURLY_BRACKET ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv218)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | AND | CLOSE | EOL | OR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : 'tv_identifier)), _, (v : 'tv_value)) = _menhir_stack in
            let _v : 'tv_simple_propositional_exp = 
# 88 "Myparser.mly"
                             (i::v)
# 862 "Myparser.ml"
             in
            _menhir_goto_simple_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv222)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv231 * _menhir_state * 'tv_identifier))) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | EOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv229 * _menhir_state * 'tv_identifier))) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : 'tv_identifier)), _, (v : 'tv_value)) = _menhir_stack in
            let _v : 'tv_primed_exp = 
# 127 "Myparser.mly"
                                    (i::v)
# 886 "Myparser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv227) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_primed_exp) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv225) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_primed_exp) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_ : 'tv_primed_exp) = _v in
            ((let _v : 'tv_temporal_formula = 
# 52 "Myparser.mly"
             (pr)
# 903 "Myparser.ml"
             in
            _menhir_goto_temporal_formula _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)) : 'freshtv230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv232)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_finite_set : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_finite_set -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_finite_set) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((fs : 'tv_finite_set) : 'tv_finite_set) = _v in
    ((let (_menhir_stack, _menhir_s, (i : 'tv_identifier)) = _menhir_stack in
    let _v : 'tv_simple_propositional_exp = 
# 89 "Myparser.mly"
                                (i::f)
# 932 "Myparser.ml"
     in
    _menhir_goto_simple_propositional_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)) : 'freshtv170)

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_identifier -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LCURLY_BRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState39 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | IDEN1 _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LSQUARE_BRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MINUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | Num ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | STR ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TRUE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv166)
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_identifier -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_reduce33 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_identifier -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : 'tv_identifier)) = _menhir_stack in
    let _v : 'tv_value = 
# 106 "Myparser.mly"
               (i)
# 1006 "Myparser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_identifier -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_boolean : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_boolean -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_boolean) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : 'tv_boolean) : 'tv_boolean) = _v in
    ((let _v : 'tv_value = 
# 109 "Myparser.mly"
            (b)
# 1047 "Myparser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)) : 'freshtv164)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv93 * _menhir_state * 'tv_identifier))) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state * 'tv_identifier))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv97 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv99 * _menhir_state) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv103 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * _menhir_state * 'tv_propositional_exp) * 'tv_logical_junction) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv111 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_nonempty_list_numeral_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv129 * _menhir_state * 'tv_value) * _menhir_state * 'tv_operator) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state * 'tv_value) * _menhir_state * 'tv_operator) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_value) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv137 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_numeral) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_value) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv145 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_value) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv147 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv149 * _menhir_state) * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv160)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_boolean = 
# 123 "Myparser.mly"
      ( etrue )
# 1233 "Myparser.ml"
     in
    _menhir_goto_boolean _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let st = () in
    let _v : 'tv_value = 
# 112 "Myparser.mly"
         (st)
# 1247 "Myparser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let n = () in
    let _v : 'tv_numeral = 
# 136 "Myparser.mly"
       (n)
# 1290 "Myparser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_numeral) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_numeral) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state) * _menhir_state * 'tv_numeral) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (n : 'tv_numeral)) = _menhir_stack in
        let _v : 'tv_value = 
# 108 "Myparser.mly"
                    (n)
# 1307 "Myparser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv78)) : 'freshtv80)
    | MenhirState84 | MenhirState0 | MenhirState68 | MenhirState73 | MenhirState3 | MenhirState61 | MenhirState56 | MenhirState52 | MenhirState48 | MenhirState39 | MenhirState40 | MenhirState42 | MenhirState5 | MenhirState34 | MenhirState13 | MenhirState23 | MenhirState26 | MenhirState19 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * 'tv_numeral) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Num ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | AND | CLOSE | COMA | EOL | LARGER | LESS | MINUS | NOTEQUAL | OR | PLUS | RCURLY_BRACKET | RSQUARE_BRACKET | TWO_PERIODS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_numeral) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_numeral)) = _menhir_stack in
            let _v : 'tv_nonempty_list_numeral_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 1325 "Myparser.ml"
             in
            _menhir_goto_nonempty_list_numeral_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv84)
    | _ ->
        _menhir_fail ()) : 'freshtv86)) : 'freshtv88)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "Myparser.mly"
      (string)
# 1393 "Myparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i1 : (
# 28 "Myparser.mly"
      (string)
# 1403 "Myparser.ml"
    )) : (
# 28 "Myparser.mly"
      (string)
# 1407 "Myparser.ml"
    )) = _v in
    ((let _v : 'tv_identifier = 
# 130 "Myparser.mly"
           (i1)
# 1412 "Myparser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_identifier) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11) : 'freshtv6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv7 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)) : 'freshtv10)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv11 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
            | LSQUARE_BRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | MINUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | Num ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | STR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv13 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)) : 'freshtv16)
    | MenhirState84 | MenhirState56 | MenhirState40 | MenhirState42 | MenhirState34 | MenhirState13 | MenhirState23 | MenhirState26 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LSQUARE_BRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | AND | CLOSE | COMA | EOL | LARGER | LESS | MINUS | NOTEQUAL | OR | PLUS | RCURLY_BRACKET | RSQUARE_BRACKET ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)) : 'freshtv20)
    | MenhirState68 | MenhirState73 | MenhirState3 | MenhirState61 | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LSQUARE_BRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | LARGER | LESS | MINUS | NOTEQUAL | PLUS ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv27 * _menhir_state * 'tv_nonempty_list_numeral_)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv25 * _menhir_state * 'tv_nonempty_list_numeral_)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (n1 : 'tv_nonempty_list_numeral_)), _, (i : 'tv_identifier)) = _menhir_stack in
        let _v : 'tv_finite_set = 
# 96 "Myparser.mly"
                                       (n1::i)
# 1529 "Myparser.ml"
         in
        _menhir_goto_finite_set _menhir_env _menhir_stack _menhir_s _v) : 'freshtv26)) : 'freshtv28)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TWO_PERIODS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | Num ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv30)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv37 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv35 * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_identifier)), _, (i2 : 'tv_identifier)) = _menhir_stack in
        let _v : 'tv_finite_set = 
# 98 "Myparser.mly"
                                          (i::i2)
# 1568 "Myparser.ml"
         in
        _menhir_goto_finite_set _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)) : 'freshtv38)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv49 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv45 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EXISTS ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | FORALL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LSQUARE_BRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | MINUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | Num ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | OPEN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | STR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv47 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)) : 'freshtv50)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv51 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv52)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)) : 'freshtv56)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv61 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv57 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EXISTS ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FORALL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | IDEN1 _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LSQUARE_BRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | MINUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | Num ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | OPEN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | STR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv58)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv59 * _menhir_state) * _menhir_state * 'tv_identifier)) * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)) : 'freshtv62)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | LSQUARE_BRACKET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PRIME ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv63 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FALSE ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | IDEN1 _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
                | LSQUARE_BRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | MINUS ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | Num ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | STR ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | TRUE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv64)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_identifier)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
        | LARGER | LESS | MINUS | NOTEQUAL | PLUS ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_identifier) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)) : 'freshtv72)
    | _ ->
        _menhir_fail ()) : 'freshtv74)) : 'freshtv76)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_boolean = 
# 122 "Myparser.mly"
       ( efalse )
# 1794 "Myparser.ml"
     in
    _menhir_goto_boolean _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

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

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 38 "Myparser.mly"
      (string * (Phrase.phrase * bool) list)
# 1826 "Myparser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXISTS ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FORALL ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDEN1 _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LSQUARE_BRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | Num ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OPEN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 269 "<standard.mly>"
  

# 1870 "Myparser.ml"
