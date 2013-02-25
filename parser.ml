type token =
  | EOF
  | INT of (Range.t * int32)
  | IDENT of (Range.t * string)
  | SEMI of (Range.t)
  | COMMA of (Range.t)
  | LBRACE of (Range.t)
  | RBRACE of (Range.t)
  | IF of (Range.t)
  | ELSE of (Range.t)
  | WHILE of (Range.t)
  | FOR of (Range.t)
  | RETURN of (Range.t)
  | TINT of (Range.t)
  | PLUS of (Range.t)
  | DASH of (Range.t)
  | STAR of (Range.t)
  | SLASH of (Range.t)
  | PERCENT of (Range.t)
  | GT of (Range.t)
  | GTEQ of (Range.t)
  | LT of (Range.t)
  | LTEQ of (Range.t)
  | EQEQ of (Range.t)
  | EQ of (Range.t)
  | BANG of (Range.t)
  | BANGEQ of (Range.t)
  | BAR of (Range.t)
  | AMPER of (Range.t)
  | LPAREN of (Range.t)
  | RPAREN of (Range.t)
  | TILDE of (Range.t)
  | LTLT of (Range.t)
  | GTGT of (Range.t)
  | GTGTGT of (Range.t)

open Parsing;;
# 2 "parser.mly"
open Ast;;
# 41 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* SEMI *);
  260 (* COMMA *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* IF *);
  264 (* ELSE *);
  265 (* WHILE *);
  266 (* FOR *);
  267 (* RETURN *);
  268 (* TINT *);
  269 (* PLUS *);
  270 (* DASH *);
  271 (* STAR *);
  272 (* SLASH *);
  273 (* PERCENT *);
  274 (* GT *);
  275 (* GTEQ *);
  276 (* LT *);
  277 (* LTEQ *);
  278 (* EQEQ *);
  279 (* EQ *);
  280 (* BANG *);
  281 (* BANGEQ *);
  282 (* BAR *);
  283 (* AMPER *);
  284 (* LPAREN *);
  285 (* RPAREN *);
  286 (* TILDE *);
  287 (* LTLT *);
  288 (* GTGT *);
  289 (* GTGTGT *);
    0|]

let yylhs = "\255\255\
\001\000\006\000\003\000\007\000\007\000\009\000\004\000\010\000\
\010\000\011\000\011\000\012\000\008\000\008\000\013\000\013\000\
\014\000\014\000\002\000\015\000\015\000\016\000\016\000\017\000\
\017\000\017\000\018\000\018\000\018\000\018\000\018\000\019\000\
\019\000\019\000\019\000\020\000\020\000\020\000\021\000\021\000\
\022\000\022\000\022\000\022\000\023\000\023\000\023\000\005\000\
\005\000\025\000\025\000\024\000\024\000\024\000\024\000\024\000\
\000\000"

let yylen = "\002\000\
\002\000\004\000\002\000\003\000\000\000\004\000\001\000\000\000\
\001\000\001\000\003\000\001\000\002\000\000\000\000\000\001\000\
\000\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\002\000\002\000\002\000\001\000\001\000\003\000\001\000\001\000\
\001\000\005\000\007\000\007\000\004\000\003\000\005\000\009\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\007\000\057\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\012\000\000\000\000\000\000\000\
\000\000\000\000\003\000\000\000\048\000\049\000\000\000\045\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\044\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\004\000\041\000\
\042\000\000\000\043\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\054\000\000\000\000\000\000\000\000\000\
\009\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\053\000\050\000\000\000\
\055\000\011\000\018\000\000\000\000\000\000\000\052\000\051\000\
\016\000\000\000\000\000\056\000"

let yydgoto = "\002\000\
\004\000\030\000\005\000\006\000\018\000\007\000\008\000\019\000\
\009\000\072\000\073\000\020\000\106\000\100\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\021\000\
\022\000"

let yysindex = "\009\000\
\001\255\000\000\000\000\000\000\020\255\035\255\039\000\080\000\
\051\255\162\255\044\255\000\000\000\000\001\255\070\255\090\255\
\110\255\080\000\000\000\054\255\000\000\000\000\001\255\000\000\
\000\000\162\255\162\255\162\255\162\255\093\255\078\255\089\255\
\242\254\104\255\015\255\047\255\121\255\000\000\000\000\162\255\
\150\255\162\255\162\255\001\255\000\000\162\255\000\000\000\000\
\000\000\129\255\000\000\000\000\162\255\162\255\162\255\162\255\
\162\255\162\255\162\255\162\255\162\255\162\255\162\255\162\255\
\162\255\162\255\000\000\000\000\133\255\136\255\174\255\179\255\
\000\000\180\255\000\000\089\255\242\254\104\255\104\255\015\255\
\015\255\015\255\015\255\047\255\047\255\047\255\121\255\121\255\
\000\000\080\000\080\000\001\255\162\255\000\000\000\000\176\255\
\000\000\000\000\000\000\182\255\080\000\080\000\000\000\000\000\
\000\000\160\255\080\000\000\000"

let yyrindex = "\000\000\
\064\000\000\000\000\000\000\000\000\000\000\000\000\000\254\254\
\000\000\000\000\000\000\000\000\000\000\074\000\000\000\000\000\
\000\000\254\254\000\000\000\000\000\000\000\000\054\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\255\224\255\
\049\255\012\000\184\255\088\255\003\255\000\000\000\000\000\000\
\000\000\000\000\000\000\190\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\194\255\000\000\
\000\000\000\000\000\000\028\000\076\255\020\000\026\000\204\255\
\216\255\236\255\248\255\108\255\128\255\148\255\037\255\068\255\
\000\000\000\000\000\000\000\000\198\255\000\000\000\000\189\255\
\000\000\000\000\000\000\000\000\000\000\183\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\200\000\000\000\192\255\000\000\192\000\198\000\
\213\255\000\000\125\000\000\000\000\000\000\000\000\000\168\000\
\178\000\018\000\085\000\022\000\107\000\232\255\000\000\199\255\
\143\000"

let yytablesize = 346
let yytable = "\050\000\
\071\000\048\000\049\000\014\000\051\000\038\000\038\000\055\000\
\014\000\001\000\056\000\067\000\003\000\069\000\070\000\038\000\
\038\000\074\000\019\000\019\000\038\000\038\000\038\000\038\000\
\038\000\095\000\097\000\038\000\038\000\038\000\010\000\038\000\
\096\000\038\000\038\000\038\000\011\000\105\000\012\000\036\000\
\036\000\089\000\108\000\103\000\019\000\061\000\062\000\063\000\
\071\000\036\000\036\000\023\000\023\000\023\000\036\000\036\000\
\036\000\036\000\036\000\064\000\065\000\036\000\036\000\036\000\
\099\000\036\000\040\000\036\000\036\000\036\000\037\000\037\000\
\078\000\079\000\023\000\023\000\046\000\023\000\022\000\022\000\
\037\000\037\000\084\000\085\000\086\000\037\000\037\000\037\000\
\037\000\037\000\035\000\035\000\037\000\037\000\037\000\052\000\
\037\000\042\000\037\000\037\000\037\000\022\000\022\000\053\000\
\022\000\035\000\035\000\035\000\035\000\035\000\032\000\032\000\
\035\000\035\000\035\000\054\000\035\000\043\000\035\000\035\000\
\035\000\057\000\058\000\059\000\060\000\032\000\032\000\032\000\
\032\000\032\000\034\000\034\000\032\000\032\000\032\000\066\000\
\032\000\044\000\032\000\032\000\032\000\080\000\081\000\082\000\
\083\000\034\000\034\000\034\000\034\000\034\000\033\000\033\000\
\034\000\034\000\034\000\068\000\034\000\075\000\034\000\034\000\
\034\000\090\000\024\000\025\000\091\000\033\000\033\000\033\000\
\033\000\033\000\087\000\088\000\033\000\033\000\033\000\026\000\
\033\000\092\000\033\000\033\000\033\000\093\000\094\000\101\000\
\102\000\027\000\031\000\031\000\107\000\028\000\048\000\029\000\
\008\000\048\000\048\000\048\000\010\000\048\000\048\000\048\000\
\017\000\031\000\031\000\031\000\031\000\031\000\029\000\029\000\
\031\000\031\000\031\000\015\000\031\000\041\000\047\000\045\000\
\098\000\048\000\030\000\030\000\076\000\029\000\029\000\029\000\
\029\000\029\000\021\000\021\000\029\000\029\000\029\000\077\000\
\029\000\030\000\030\000\030\000\030\000\030\000\027\000\027\000\
\030\000\030\000\030\000\104\000\030\000\000\000\000\000\000\000\
\000\000\021\000\028\000\028\000\021\000\027\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\027\000\000\000\
\027\000\028\000\028\000\028\000\028\000\028\000\026\000\026\000\
\028\000\028\000\028\000\000\000\028\000\000\000\024\000\024\000\
\000\000\000\000\000\000\000\000\025\000\025\000\020\000\020\000\
\000\000\026\000\000\000\000\000\026\000\026\000\026\000\000\000\
\026\000\024\000\000\000\000\000\024\000\024\000\024\000\025\000\
\024\000\000\000\025\000\025\000\025\000\020\000\025\000\005\000\
\020\000\000\000\005\000\005\000\005\000\000\000\005\000\005\000\
\005\000\005\000\000\000\000\000\005\000\000\000\005\000\000\000\
\005\000\005\000\005\000\005\000\000\000\000\000\005\000\005\000\
\005\000\013\000\005\000\005\000\014\000\000\000\015\000\000\000\
\016\000\017\000"

let yycheck = "\028\000\
\044\000\026\000\027\000\006\001\029\000\003\001\004\001\022\001\
\011\001\001\000\025\001\040\000\012\001\042\000\043\000\013\001\
\014\001\046\000\003\001\004\001\018\001\019\001\020\001\021\001\
\022\001\090\000\091\000\025\001\026\001\027\001\011\001\029\001\
\090\000\031\001\032\001\033\001\002\001\102\000\000\000\003\001\
\004\001\066\000\107\000\101\000\029\001\031\001\032\001\033\001\
\092\000\013\001\014\001\003\001\004\001\003\001\018\001\019\001\
\020\001\021\001\022\001\013\001\014\001\025\001\026\001\027\001\
\093\000\029\001\023\001\031\001\032\001\033\001\003\001\004\001\
\055\000\056\000\026\001\027\001\023\001\029\001\003\001\004\001\
\013\001\014\001\061\000\062\000\063\000\018\001\019\001\020\001\
\021\001\022\001\003\001\004\001\025\001\026\001\027\001\003\001\
\029\001\028\001\031\001\032\001\033\001\026\001\027\001\026\001\
\029\001\018\001\019\001\020\001\021\001\022\001\003\001\004\001\
\025\001\026\001\027\001\027\001\029\001\028\001\031\001\032\001\
\033\001\018\001\019\001\020\001\021\001\018\001\019\001\020\001\
\021\001\022\001\003\001\004\001\025\001\026\001\027\001\015\001\
\029\001\028\001\031\001\032\001\033\001\057\000\058\000\059\000\
\060\000\018\001\019\001\020\001\021\001\022\001\003\001\004\001\
\025\001\026\001\027\001\006\001\029\001\029\001\031\001\032\001\
\033\001\029\001\001\001\002\001\029\001\018\001\019\001\020\001\
\021\001\022\001\064\000\065\000\025\001\026\001\027\001\014\001\
\029\001\004\001\031\001\032\001\033\001\003\001\003\001\008\001\
\003\001\024\001\003\001\004\001\029\001\028\001\002\001\030\001\
\003\001\005\001\006\001\007\001\003\001\009\001\010\001\011\001\
\003\001\018\001\019\001\020\001\021\001\022\001\003\001\004\001\
\025\001\026\001\027\001\029\001\029\001\014\000\023\000\018\000\
\092\000\029\001\003\001\004\001\053\000\018\001\019\001\020\001\
\021\001\022\001\003\001\004\001\025\001\026\001\027\001\054\000\
\029\001\018\001\019\001\020\001\021\001\022\001\003\001\004\001\
\025\001\026\001\027\001\101\000\029\001\255\255\255\255\255\255\
\255\255\026\001\003\001\004\001\029\001\018\001\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001\026\001\027\001\255\255\
\029\001\018\001\019\001\020\001\021\001\022\001\003\001\004\001\
\025\001\026\001\027\001\255\255\029\001\255\255\003\001\004\001\
\255\255\255\255\255\255\255\255\003\001\004\001\003\001\004\001\
\255\255\022\001\255\255\255\255\025\001\026\001\027\001\255\255\
\029\001\022\001\255\255\255\255\025\001\026\001\027\001\022\001\
\029\001\255\255\025\001\026\001\027\001\026\001\029\001\002\001\
\029\001\255\255\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\002\001\255\255\255\255\005\001\255\255\007\001\255\255\
\009\001\010\001\011\001\002\001\255\255\255\255\005\001\006\001\
\007\001\002\001\009\001\010\001\005\001\255\255\007\001\255\255\
\009\001\010\001"

let yynames_const = "\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  SEMI\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  TINT\000\
  PLUS\000\
  DASH\000\
  STAR\000\
  SLASH\000\
  PERCENT\000\
  GT\000\
  GTEQ\000\
  LT\000\
  LTEQ\000\
  EQEQ\000\
  EQ\000\
  BANG\000\
  BANGEQ\000\
  BAR\000\
  AMPER\000\
  LPAREN\000\
  RPAREN\000\
  TILDE\000\
  LTLT\000\
  GTGT\000\
  GTGTGT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 51 "parser.mly"
             ( _1 )
# 302 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.block) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 55 "parser.mly"
                          ( (_1, _3) )
# 312 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 58 "parser.mly"
                 ( (_1, _2) )
# 320 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecls) in
    Obj.repr(
# 61 "parser.mly"
                      ( _1::_3 )
# 329 "parser.ml"
               : 'vdecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                ( [] )
# 335 "parser.ml"
               : 'vdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 65 "parser.mly"
                      ( {v_ty=_1; v_id=snd(_2); v_init=_4 } )
# 345 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 68 "parser.mly"
         ( TInt )
# 352 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                ( [] )
# 358 "parser.ml"
               : 'vdecllist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdeclplus) in
    Obj.repr(
# 73 "parser.mly"
              ( _1 )
# 365 "parser.ml"
               : 'vdecllist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 76 "parser.mly"
            ( [_1] )
# 372 "parser.ml"
               : 'vdeclplus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdeclplus) in
    Obj.repr(
# 77 "parser.mly"
                          ( _1::_3 )
# 381 "parser.ml"
               : 'vdeclplus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * string) in
    Obj.repr(
# 80 "parser.mly"
          ( _1 )
# 388 "parser.ml"
               : 'lhs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 83 "parser.mly"
               ( _1::_2 )
# 396 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                 ( [] )
# 402 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                 ( None )
# 408 "parser.ml"
               : 'stmtOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 88 "parser.mly"
                 ( Some _1 )
# 415 "parser.ml"
               : 'stmtOPT))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                 ( None )
# 421 "parser.ml"
               : 'expOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 92 "parser.mly"
                 ( Some _1 )
# 428 "parser.ml"
               : 'expOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E4) in
    Obj.repr(
# 95 "parser.mly"
       ( _1 )
# 435 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E4) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E5) in
    Obj.repr(
# 98 "parser.mly"
              ( Binop (Or, _1, _3) )
# 444 "parser.ml"
               : 'E4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E5) in
    Obj.repr(
# 99 "parser.mly"
       ( _1 )
# 451 "parser.ml"
               : 'E4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E5) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E6) in
    Obj.repr(
# 102 "parser.mly"
                ( Binop (And, _1, _3) )
# 460 "parser.ml"
               : 'E5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E6) in
    Obj.repr(
# 103 "parser.mly"
       ( _1 )
# 467 "parser.ml"
               : 'E5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 106 "parser.mly"
               ( Binop (Eq, _1, _3) )
# 476 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 107 "parser.mly"
                 ( Binop (Neq, _1, _3) )
# 485 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 108 "parser.mly"
       ( _1 )
# 492 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 111 "parser.mly"
             ( Binop (Lt, _1, _3) )
# 501 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 112 "parser.mly"
               ( Binop (Lte, _1, _3) )
# 510 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 113 "parser.mly"
             ( Binop (Gt, _1, _3) )
# 519 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 114 "parser.mly"
               ( Binop (Gte, _1, _3) )
# 528 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 115 "parser.mly"
       ( _1 )
# 535 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 118 "parser.mly"
               ( Binop (Shl, _1, _3) )
# 544 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 119 "parser.mly"
                 ( Binop (Shr, _1, _3) )
# 553 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 120 "parser.mly"
               ( Binop (Sar, _1, _3) )
# 562 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 121 "parser.mly"
       ( _1 )
# 569 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E9) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 124 "parser.mly"
                ( Binop (Plus, _1, _3) )
# 578 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E9) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 125 "parser.mly"
                ( Binop (Minus, _1, _3) )
# 587 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 126 "parser.mly"
        ( _1 )
# 594 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E10) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 129 "parser.mly"
                 ( Binop (Times, _1, _3) )
# 603 "parser.ml"
               : 'E10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 130 "parser.mly"
        ( _1 )
# 610 "parser.ml"
               : 'E10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 133 "parser.mly"
             ( Unop (Neg, _2) )
# 618 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 134 "parser.mly"
             ( Unop (Lognot, _2) )
# 626 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 135 "parser.mly"
              ( Unop (Not, _2) )
# 634 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E12) in
    Obj.repr(
# 136 "parser.mly"
        ( _1 )
# 641 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * int32) in
    Obj.repr(
# 139 "parser.mly"
        ( Cint (snd _1) )
# 648 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 140 "parser.mly"
                      ( _2 )
# 657 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * string) in
    Obj.repr(
# 141 "parser.mly"
            ( Id (snd _1) )
# 664 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_mIF) in
    Obj.repr(
# 144 "parser.mly"
             ( _1 )
# 671 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_uIF) in
    Obj.repr(
# 145 "parser.mly"
            ( _1 )
# 678 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 148 "parser.mly"
                                         ( If (_3, _5, None) )
# 689 "parser.ml"
               : 'stmt_uIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_mIF) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_uIF) in
    Obj.repr(
# 149 "parser.mly"
                                                ( If (_3, _5, Some (_7)) )
# 702 "parser.ml"
               : 'stmt_uIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_mIF) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_mIF) in
    Obj.repr(
# 153 "parser.mly"
                                               ( If (_3, _5, Some (_7)) )
# 715 "parser.ml"
               : 'stmt_mIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lhs) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 154 "parser.mly"
                                ( Assign (Var(snd(_1)), _3) )
# 725 "parser.ml"
               : 'stmt_mIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 155 "parser.mly"
                                  ( Block (_2) )
# 734 "parser.ml"
               : 'stmt_mIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 156 "parser.mly"
                                       ( While (_3, _5) )
# 745 "parser.ml"
               : 'stmt_mIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'vdecllist) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expOPT) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'stmtOPT) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 157 "parser.mly"
                                                             ( For (_3,_5,_7,_9) )
# 760 "parser.ml"
               : 'stmt_mIF))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
