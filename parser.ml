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
\005\000\005\000\024\000\024\000\024\000\026\000\026\000\025\000\
\025\000\000\000"

let yylen = "\002\000\
\002\000\004\000\002\000\003\000\000\000\004\000\001\000\000\000\
\001\000\001\000\003\000\001\000\002\000\000\000\000\000\001\000\
\000\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\002\000\002\000\002\000\001\000\001\000\003\000\001\000\001\000\
\001\000\001\000\004\000\005\000\009\000\005\000\007\000\007\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\007\000\058\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\012\000\000\000\000\000\000\000\
\000\000\003\000\000\000\048\000\049\000\050\000\000\000\045\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\044\000\000\000\
\000\000\000\000\000\000\013\000\000\000\004\000\041\000\042\000\
\000\000\043\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\009\000\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\051\000\054\000\000\000\052\000\011\000\
\018\000\000\000\000\000\000\000\057\000\056\000\055\000\016\000\
\000\000\000\000\053\000"

let yydgoto = "\002\000\
\004\000\030\000\005\000\006\000\017\000\007\000\008\000\018\000\
\009\000\070\000\071\000\019\000\105\000\098\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\020\000\
\021\000\022\000"

let yysindex = "\005\000\
\255\254\000\000\000\000\000\000\009\255\014\255\033\000\025\000\
\032\255\164\255\016\255\000\000\000\000\023\255\028\255\034\255\
\025\000\000\000\044\255\000\000\000\000\000\000\255\254\000\000\
\000\000\164\255\164\255\164\255\164\255\066\255\050\255\052\255\
\241\254\106\255\054\255\092\255\083\255\000\000\000\000\164\255\
\164\255\164\255\255\254\000\000\164\255\000\000\000\000\000\000\
\071\255\000\000\000\000\164\255\164\255\164\255\164\255\164\255\
\164\255\164\255\164\255\164\255\164\255\164\255\164\255\164\255\
\164\255\000\000\089\255\091\255\134\255\137\255\000\000\155\255\
\000\000\052\255\241\254\106\255\106\255\054\255\054\255\054\255\
\054\255\092\255\092\255\092\255\083\255\083\255\000\000\025\000\
\025\000\255\254\164\255\000\000\000\000\152\255\000\000\000\000\
\000\000\161\255\025\000\025\000\000\000\000\000\000\000\000\000\
\138\255\025\000\000\000"

let yyrindex = "\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\169\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\169\255\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\255\078\255\
\018\255\255\255\182\255\090\255\005\255\000\000\000\000\000\000\
\000\000\000\000\181\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\184\255\000\000\000\000\000\000\
\000\000\026\000\051\255\011\000\019\000\202\255\215\255\235\255\
\247\255\110\255\130\255\150\255\039\255\070\255\000\000\000\000\
\000\000\000\000\186\255\000\000\000\000\188\255\000\000\000\000\
\000\000\000\000\000\000\162\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\000\000\000\000\196\255\000\000\170\000\179\000\
\214\255\000\000\120\000\000\000\000\000\000\000\000\000\163\000\
\177\000\119\000\088\000\152\000\162\000\232\255\000\000\117\000\
\172\255\133\000"

let yytablesize = 311
let yytable = "\049\000\
\069\000\047\000\048\000\094\000\050\000\001\000\054\000\038\000\
\038\000\055\000\003\000\066\000\067\000\068\000\102\000\011\000\
\072\000\038\000\038\000\010\000\023\000\023\000\038\000\038\000\
\038\000\038\000\038\000\093\000\095\000\038\000\038\000\038\000\
\012\000\038\000\023\000\038\000\038\000\038\000\040\000\104\000\
\087\000\036\000\036\000\023\000\023\000\107\000\023\000\069\000\
\019\000\019\000\041\000\036\000\036\000\022\000\022\000\042\000\
\036\000\036\000\036\000\036\000\036\000\043\000\097\000\036\000\
\036\000\036\000\045\000\036\000\051\000\036\000\036\000\036\000\
\037\000\037\000\019\000\052\000\022\000\022\000\053\000\022\000\
\021\000\021\000\037\000\037\000\060\000\061\000\062\000\037\000\
\037\000\037\000\037\000\037\000\035\000\035\000\037\000\037\000\
\037\000\065\000\037\000\073\000\037\000\037\000\037\000\021\000\
\063\000\064\000\021\000\035\000\035\000\035\000\035\000\035\000\
\032\000\032\000\035\000\035\000\035\000\088\000\035\000\089\000\
\035\000\035\000\035\000\056\000\057\000\058\000\059\000\032\000\
\032\000\032\000\032\000\032\000\034\000\034\000\032\000\032\000\
\032\000\090\000\032\000\091\000\032\000\032\000\032\000\078\000\
\079\000\080\000\081\000\034\000\034\000\034\000\034\000\034\000\
\033\000\033\000\034\000\034\000\034\000\092\000\034\000\099\000\
\034\000\034\000\034\000\100\000\024\000\025\000\106\000\033\000\
\033\000\033\000\033\000\033\000\076\000\077\000\033\000\033\000\
\033\000\026\000\033\000\014\000\033\000\033\000\033\000\008\000\
\031\000\031\000\010\000\027\000\017\000\049\000\015\000\028\000\
\046\000\029\000\049\000\044\000\049\000\049\000\049\000\031\000\
\031\000\031\000\031\000\031\000\029\000\029\000\031\000\031\000\
\031\000\096\000\031\000\082\000\083\000\084\000\074\000\101\000\
\049\000\030\000\030\000\029\000\029\000\029\000\029\000\029\000\
\085\000\086\000\029\000\029\000\029\000\075\000\029\000\103\000\
\030\000\030\000\030\000\030\000\030\000\027\000\027\000\030\000\
\030\000\030\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\028\000\028\000\000\000\027\000\027\000\027\000\027\000\
\027\000\026\000\026\000\027\000\027\000\027\000\000\000\027\000\
\028\000\028\000\028\000\028\000\028\000\024\000\024\000\028\000\
\028\000\028\000\000\000\028\000\026\000\025\000\025\000\026\000\
\026\000\026\000\013\000\026\000\020\000\020\000\000\000\014\000\
\024\000\015\000\016\000\024\000\024\000\024\000\000\000\024\000\
\025\000\005\000\000\000\025\000\025\000\025\000\005\000\025\000\
\005\000\005\000\005\000\020\000\000\000\000\000\020\000"

let yycheck = "\028\000\
\043\000\026\000\027\000\088\000\029\000\001\000\022\001\003\001\
\004\001\025\001\012\001\040\000\041\000\042\000\099\000\002\001\
\045\000\013\001\014\001\011\001\003\001\004\001\018\001\019\001\
\020\001\021\001\022\001\088\000\089\000\025\001\026\001\027\001\
\000\000\029\001\003\001\031\001\032\001\033\001\023\001\100\000\
\065\000\003\001\004\001\026\001\027\001\106\000\029\001\090\000\
\003\001\004\001\028\001\013\001\014\001\003\001\004\001\028\001\
\018\001\019\001\020\001\021\001\022\001\028\001\091\000\025\001\
\026\001\027\001\023\001\029\001\003\001\031\001\032\001\033\001\
\003\001\004\001\029\001\026\001\026\001\027\001\027\001\029\001\
\003\001\004\001\013\001\014\001\031\001\032\001\033\001\018\001\
\019\001\020\001\021\001\022\001\003\001\004\001\025\001\026\001\
\027\001\015\001\029\001\029\001\031\001\032\001\033\001\026\001\
\013\001\014\001\029\001\018\001\019\001\020\001\021\001\022\001\
\003\001\004\001\025\001\026\001\027\001\029\001\029\001\029\001\
\031\001\032\001\033\001\018\001\019\001\020\001\021\001\018\001\
\019\001\020\001\021\001\022\001\003\001\004\001\025\001\026\001\
\027\001\004\001\029\001\003\001\031\001\032\001\033\001\056\000\
\057\000\058\000\059\000\018\001\019\001\020\001\021\001\022\001\
\003\001\004\001\025\001\026\001\027\001\003\001\029\001\008\001\
\031\001\032\001\033\001\003\001\001\001\002\001\029\001\018\001\
\019\001\020\001\021\001\022\001\054\000\055\000\025\001\026\001\
\027\001\014\001\029\001\011\001\031\001\032\001\033\001\003\001\
\003\001\004\001\003\001\024\001\003\001\002\001\029\001\028\001\
\023\000\030\001\007\001\017\000\009\001\010\001\011\001\018\001\
\019\001\020\001\021\001\022\001\003\001\004\001\025\001\026\001\
\027\001\090\000\029\001\060\000\061\000\062\000\052\000\099\000\
\029\001\003\001\004\001\018\001\019\001\020\001\021\001\022\001\
\063\000\064\000\025\001\026\001\027\001\053\000\029\001\099\000\
\018\001\019\001\020\001\021\001\022\001\003\001\004\001\025\001\
\026\001\027\001\255\255\029\001\255\255\255\255\255\255\255\255\
\255\255\003\001\004\001\255\255\018\001\019\001\020\001\021\001\
\022\001\003\001\004\001\025\001\026\001\027\001\255\255\029\001\
\018\001\019\001\020\001\021\001\022\001\003\001\004\001\025\001\
\026\001\027\001\255\255\029\001\022\001\003\001\004\001\025\001\
\026\001\027\001\002\001\029\001\003\001\004\001\255\255\007\001\
\022\001\009\001\010\001\025\001\026\001\027\001\255\255\029\001\
\022\001\002\001\255\255\025\001\026\001\027\001\007\001\029\001\
\009\001\010\001\011\001\026\001\255\255\255\255\029\001"

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
# 292 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.block) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 55 "parser.mly"
                          ( (_1, _3) )
# 302 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 58 "parser.mly"
                 ( (_1, _2) )
# 310 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecls) in
    Obj.repr(
# 61 "parser.mly"
                      ( _1::_3 )
# 319 "parser.ml"
               : 'vdecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                ( [] )
# 325 "parser.ml"
               : 'vdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ast.typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t * string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 65 "parser.mly"
                      ( {v_ty=_1; v_id=snd(_2); v_init=_4 } )
# 335 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 68 "parser.mly"
         ( TInt )
# 342 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                ( [] )
# 348 "parser.ml"
               : 'vdecllist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdeclplus) in
    Obj.repr(
# 73 "parser.mly"
              ( _1 )
# 355 "parser.ml"
               : 'vdecllist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 76 "parser.mly"
            ( [_1] )
# 362 "parser.ml"
               : 'vdeclplus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdeclplus) in
    Obj.repr(
# 77 "parser.mly"
                          ( _1::_3 )
# 371 "parser.ml"
               : 'vdeclplus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * string) in
    Obj.repr(
# 80 "parser.mly"
          ( _1 )
# 378 "parser.ml"
               : 'lhs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 83 "parser.mly"
               ( _1::_2 )
# 386 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                 ( [] )
# 392 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                 ( None )
# 398 "parser.ml"
               : 'stmtOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 88 "parser.mly"
                 ( Some _1 )
# 405 "parser.ml"
               : 'stmtOPT))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                 ( None )
# 411 "parser.ml"
               : 'expOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 92 "parser.mly"
                 ( Some _1 )
# 418 "parser.ml"
               : 'expOPT))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E4) in
    Obj.repr(
# 95 "parser.mly"
       ( _1 )
# 425 "parser.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E4) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E5) in
    Obj.repr(
# 98 "parser.mly"
              ( Binop (Or, _1, _3) )
# 434 "parser.ml"
               : 'E4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E5) in
    Obj.repr(
# 99 "parser.mly"
       ( _1 )
# 441 "parser.ml"
               : 'E4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E5) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E6) in
    Obj.repr(
# 102 "parser.mly"
                ( Binop (And, _1, _3) )
# 450 "parser.ml"
               : 'E5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E6) in
    Obj.repr(
# 103 "parser.mly"
       ( _1 )
# 457 "parser.ml"
               : 'E5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 106 "parser.mly"
               ( Binop (Eq, _1, _3) )
# 466 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E6) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 107 "parser.mly"
                 ( Binop (Neq, _1, _3) )
# 475 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E7) in
    Obj.repr(
# 108 "parser.mly"
       ( _1 )
# 482 "parser.ml"
               : 'E6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 111 "parser.mly"
             ( Binop (Lt, _1, _3) )
# 491 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 112 "parser.mly"
               ( Binop (Lte, _1, _3) )
# 500 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 113 "parser.mly"
             ( Binop (Gt, _1, _3) )
# 509 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E7) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 114 "parser.mly"
               ( Binop (Gte, _1, _3) )
# 518 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E8) in
    Obj.repr(
# 115 "parser.mly"
       ( _1 )
# 525 "parser.ml"
               : 'E7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 118 "parser.mly"
               ( Binop (Shl, _1, _3) )
# 534 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 119 "parser.mly"
                 ( Binop (Shr, _1, _3) )
# 543 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E8) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 120 "parser.mly"
               ( Binop (Sar, _1, _3) )
# 552 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E9) in
    Obj.repr(
# 121 "parser.mly"
       ( _1 )
# 559 "parser.ml"
               : 'E8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E9) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 124 "parser.mly"
                ( Binop (Plus, _1, _3) )
# 568 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E9) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 125 "parser.mly"
                ( Binop (Minus, _1, _3) )
# 577 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E10) in
    Obj.repr(
# 126 "parser.mly"
        ( _1 )
# 584 "parser.ml"
               : 'E9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E10) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 129 "parser.mly"
                 ( Binop (Times, _1, _3) )
# 593 "parser.ml"
               : 'E10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 130 "parser.mly"
        ( _1 )
# 600 "parser.ml"
               : 'E10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 133 "parser.mly"
             ( Unop (Neg, _2) )
# 608 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 134 "parser.mly"
             ( Unop (Lognot, _2) )
# 616 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'E11) in
    Obj.repr(
# 135 "parser.mly"
              ( Unop (Not, _2) )
# 624 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'E12) in
    Obj.repr(
# 136 "parser.mly"
        ( _1 )
# 631 "parser.ml"
               : 'E11))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * int32) in
    Obj.repr(
# 139 "parser.mly"
        ( Cint (snd _1) )
# 638 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 140 "parser.mly"
                      ( _2 )
# 647 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Range.t * string) in
    Obj.repr(
# 141 "parser.mly"
            ( Id (snd _1) )
# 654 "parser.ml"
               : 'E12))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_noIF) in
    Obj.repr(
# 145 "parser.mly"
             ( _1 )
# 661 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_mIF) in
    Obj.repr(
# 146 "parser.mly"
             ( _1 )
# 668 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_uIF) in
    Obj.repr(
# 147 "parser.mly"
            ( _1 )
# 675 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lhs) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Range.t) in
    Obj.repr(
# 150 "parser.mly"
                   ( Assign (Var(snd(_1)), _3) )
# 685 "parser.ml"
               : 'stmt_noIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 151 "parser.mly"
                                ( While (_3, _5) )
# 696 "parser.ml"
               : 'stmt_noIF))
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
# 152 "parser.mly"
                                                             ( For (_3,_5,_7,_9) )
# 711 "parser.ml"
               : 'stmt_noIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Range.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Range.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Range.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 156 "parser.mly"
                             ( IF(_3, _5, None) )
# 722 "parser.ml"
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
# 157 "parser.mly"
                                               ( If (_3, _5, Some (_7)) )
# 735 "parser.ml"
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
# 161 "parser.mly"
                                               ( If (_3, _5, Some (_7)) )
# 748 "parser.ml"
               : 'stmt_mIF))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_noIF) in
    Obj.repr(
# 162 "parser.mly"
             ( _1 )
# 755 "parser.ml"
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
