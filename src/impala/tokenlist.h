#ifndef IMPALA_PREFIX
#define IMPALA_PREFIX(tok, str, prec)
#endif

IMPALA_PREFIX(  ADD,   "+", POSTFIX) // unary +
IMPALA_PREFIX(  SUB,   "-", POSTFIX) // unary -
IMPALA_PREFIX(  MUL,   "*", POSTFIX) // deref
IMPALA_PREFIX(  AND,   "&", POSTFIX) // address of
IMPALA_PREFIX(TILDE,   "~", POSTFIX) // owned ptr constructor
IMPALA_PREFIX(  NOT,   "!", POSTFIX) // not
IMPALA_PREFIX(  INC,  "++", POSTFIX) // prefix ++
IMPALA_PREFIX(  DEC,  "--", POSTFIX) // prefix --
IMPALA_PREFIX(   OR,   "|", POSTFIX) // lambda expressions
IMPALA_PREFIX( OROR,  "||", POSTFIX) // lambda expressions with empty param list
IMPALA_PREFIX(  RUN,   "@",    EVAL) // trigger partial evaluation
IMPALA_PREFIX(  HLT,   "$",    EVAL) // stop partial evaluation

#undef IMPALA_PREFIX

#ifndef IMPALA_POSTFIX
#define IMPALA_POSTFIX(tok, str, prec)
#endif

IMPALA_POSTFIX(      INC, "++", POSTFIX) // postfix ++
IMPALA_POSTFIX(      DEC, "--", POSTFIX) // postfix --
IMPALA_POSTFIX(L_BRACKET,  "[", POSTFIX) // map expression with type argument list
IMPALA_POSTFIX(  L_PAREN,  "(", POSTFIX) // map expression (function call, array/tuple index)
IMPALA_POSTFIX(      DOT,  ".", POSTFIX) // dot expression (struct access)
IMPALA_POSTFIX(       AS, "as",     MUL) // cast expression (not as strong as mul)

#undef IMPALA_POSTFIX

#ifndef IMPALA_INFIX_ASGN
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec)
#endif

IMPALA_INFIX_ASGN(    ASGN,   "=", COND, ASGN)
IMPALA_INFIX_ASGN(ADD_ASGN,  "+=", COND, ASGN)
IMPALA_INFIX_ASGN(SUB_ASGN,  "-=", COND, ASGN)
IMPALA_INFIX_ASGN(MUL_ASGN,  "*=", COND, ASGN)
IMPALA_INFIX_ASGN(DIV_ASGN,  "/=", COND, ASGN)
IMPALA_INFIX_ASGN(REM_ASGN,  "%=", COND, ASGN)
IMPALA_INFIX_ASGN(AND_ASGN,  "&=", COND, ASGN)
IMPALA_INFIX_ASGN( OR_ASGN,  "|=", COND, ASGN)
IMPALA_INFIX_ASGN(XOR_ASGN,  "^=", COND, ASGN)
IMPALA_INFIX_ASGN(SHL_ASGN, "<<=", COND, ASGN)
IMPALA_INFIX_ASGN(SHR_ASGN, ">>=", COND, ASGN)

#undef IMPALA_INFIX_ASGN

#ifndef IMPALA_INFIX
#define IMPALA_INFIX(tok, str, lprec, rprec)
#endif

IMPALA_INFIX(  OROR, "||",   OROR, ANDAND)
IMPALA_INFIX(ANDAND, "&&", ANDAND,     OR)
IMPALA_INFIX(    OR,  "|",     OR,    XOR)
IMPALA_INFIX(   XOR,  "^",    XOR,    AND)
IMPALA_INFIX(   AND,  "&",    AND,     EQ)
IMPALA_INFIX(    EQ, "==",     EQ,    REL)
IMPALA_INFIX(    NE, "!=",     EQ,    REL)
IMPALA_INFIX(    LT,  "<",    REL,  SHIFT)
IMPALA_INFIX(    LE, "<=",    REL,  SHIFT)
IMPALA_INFIX(    GT,  ">",    REL,  SHIFT)
IMPALA_INFIX(    GE, ">=",    REL,  SHIFT)
IMPALA_INFIX(   SHL, "<<",  SHIFT,    ADD)
IMPALA_INFIX(   SHR, ">>",  SHIFT,    ADD)
IMPALA_INFIX(   ADD,  "+",    ADD,    MUL)
IMPALA_INFIX(   SUB,  "-",    ADD,    MUL)
IMPALA_INFIX(   MUL,  "*",    MUL,  UNARY)
IMPALA_INFIX(   DIV,  "/",    MUL,  UNARY)
IMPALA_INFIX(   REM,  "%",    MUL,  UNARY)

#undef IMPALA_INFIX

#ifndef IMPALA_KEY
#define IMPALA_KEY(tok, str)
#endif

// Windows workaround for MSVC18
#undef IN

IMPALA_KEY(AS,        "as")
IMPALA_KEY(DO,        "do")
IMPALA_KEY(ELSE,      "else")
IMPALA_KEY(ENUM,      "enum")
IMPALA_KEY(EXTERN,    "extern")
IMPALA_KEY(FALSE,     "false")
IMPALA_KEY(FN,        "fn")
IMPALA_KEY(FOR,       "for")
IMPALA_KEY(WITH,      "with")
IMPALA_KEY(IF,        "if")
IMPALA_KEY(IMPL,      "impl")
IMPALA_KEY(IN,        "in")
IMPALA_KEY(INTRINSIC, "intrinsic")
IMPALA_KEY(LET,       "let")
IMPALA_KEY(MOD,       "mod")
IMPALA_KEY(PRIV,      "priv")
IMPALA_KEY(PUB,       "pub")
IMPALA_KEY(STATIC,    "static")
IMPALA_KEY(STRUCT,    "struct")
IMPALA_KEY(TRAIT,     "trait")
IMPALA_KEY(TRUE,      "true")
IMPALA_KEY(TYPEDEF,   "type")
IMPALA_KEY(TYPEOF,    "typeof")
IMPALA_KEY(WHILE,     "while")
IMPALA_KEY(SIMD,      "simd")

#undef IMPALA_KEY

#ifndef IMPALA_MISC
#define IMPALA_MISC(tok, str)
#endif

IMPALA_MISC(L_PAREN,      "(")
IMPALA_MISC(R_PAREN,      ")")
IMPALA_MISC(L_BRACE,      "{")
IMPALA_MISC(R_BRACE,      "}")
IMPALA_MISC(L_BRACKET,    "[")
IMPALA_MISC(R_BRACKET,    "]")
IMPALA_MISC(RUN_BLOCK,    "@{")
IMPALA_MISC(ARROW,        "->")
IMPALA_MISC(FAT_ARRROW,   "=>")
IMPALA_MISC(SEMICOLON,    ";")
IMPALA_MISC(COLON,        ":")
IMPALA_MISC(DOUBLE_COLON, ":")
IMPALA_MISC(COMMA,        ",")
IMPALA_MISC(DOTDOT,       "..")

#undef IMPALA_MISC

#ifndef IMPALA_LIT
#define IMPALA_LIT(itype, atype)
#endif

IMPALA_LIT(i8,  qs8)
IMPALA_LIT(i16, qs16)
IMPALA_LIT(i32, qs32)
IMPALA_LIT(i64, qs64)
IMPALA_LIT(u8,  pu8)
IMPALA_LIT(u16, pu16)
IMPALA_LIT(u32, pu32)
IMPALA_LIT(u64, pu64)
IMPALA_LIT(f32, pf32)
IMPALA_LIT(f64, pf64)

#undef IMPALA_LIT

#ifndef IMPALA_TYPE
#define IMPALA_TYPE(itype, atype)
#endif

IMPALA_TYPE(i8,   qs8)
IMPALA_TYPE(i16,  qs16)
IMPALA_TYPE(i32,  qs32)
IMPALA_TYPE(i64,  qs64)
IMPALA_TYPE(u8,   pu8)
IMPALA_TYPE(u16,  pu16)
IMPALA_TYPE(u32,  pu32)
IMPALA_TYPE(u64,  pu64)
IMPALA_TYPE(f32,  pf32)
IMPALA_TYPE(f64,  pf64)
IMPALA_TYPE(bool, bool)

#undef IMPALA_TYPE
