#ifndef IMPALA_PREFIX
#define IMPALA_PREFIX(tok, str)
#endif

IMPALA_PREFIX(  ADD,   "+") // unary +
IMPALA_PREFIX(  SUB,   "-") // unary -
IMPALA_PREFIX(  MUL,   "*") // deref
IMPALA_PREFIX(  AND,   "&") // address of
IMPALA_PREFIX(TILDE,   "~") // owned ptr constructor
IMPALA_PREFIX(  NOT,   "!") // not
IMPALA_PREFIX(  INC,  "++") // prefix ++
IMPALA_PREFIX(  DEC,  "--") // prefix --
IMPALA_PREFIX(   OR,   "|") // lambda expressions
IMPALA_PREFIX( OROR,  "||") // lambda expressions with empty param list
IMPALA_PREFIX(  RUN,   "@") // lambda expressions with pe profile
IMPALA_PREFIX(  HLT,   "$") // stop partial evaluation
IMPALA_PREFIX(KNOWN,   "?") // is value statically known?

#undef IMPALA_PREFIX

#ifndef IMPALA_POSTFIX
#define IMPALA_POSTFIX(tok, str)
#endif

IMPALA_POSTFIX(      INC, "++") // postfix ++
IMPALA_POSTFIX(      DEC, "--") // postfix --
IMPALA_POSTFIX(L_BRACKET,  "[") // MapExpr with type argument list
IMPALA_POSTFIX(  L_PAREN,  "(") // MapExpr (function call, array/tuple index)
IMPALA_POSTFIX(      DOT,  ".") // FieldExpr

#undef IMPALA_POSTFIX

#ifndef IMPALA_INFIX_ASGN
#define IMPALA_INFIX_ASGN(tok, str)
#endif

IMPALA_INFIX_ASGN(    ASGN,   "=")
IMPALA_INFIX_ASGN(ADD_ASGN,  "+=")
IMPALA_INFIX_ASGN(SUB_ASGN,  "-=")
IMPALA_INFIX_ASGN(MUL_ASGN,  "*=")
IMPALA_INFIX_ASGN(DIV_ASGN,  "/=")
IMPALA_INFIX_ASGN(REM_ASGN,  "%=")
IMPALA_INFIX_ASGN(AND_ASGN,  "&=")
IMPALA_INFIX_ASGN( OR_ASGN,  "|=")
IMPALA_INFIX_ASGN(XOR_ASGN,  "^=")
IMPALA_INFIX_ASGN(SHL_ASGN, "<<=")
IMPALA_INFIX_ASGN(SHR_ASGN, ">>=")

#undef IMPALA_INFIX_ASGN

#ifndef IMPALA_INFIX
#define IMPALA_INFIX(tok, str, prec)
#endif

IMPALA_INFIX(  OROR, "||",   OrOr)
IMPALA_INFIX(ANDAND, "&&", AndAnd)
IMPALA_INFIX(    EQ, "==",    Rel)
IMPALA_INFIX(    NE, "!=",    Rel)
IMPALA_INFIX(    LT,  "<",    Rel)
IMPALA_INFIX(    LE, "<=",    Rel)
IMPALA_INFIX(    GT,  ">",    Rel)
IMPALA_INFIX(    GE, ">=",    Rel)
IMPALA_INFIX(    OR,  "|",     Or)
IMPALA_INFIX(   XOR,  "^",    Xor)
IMPALA_INFIX(   AND,  "&",    And)
IMPALA_INFIX(   SHL, "<<",  Shift)
IMPALA_INFIX(   SHR, ">>",  Shift)
IMPALA_INFIX(   ADD,  "+",    Add)
IMPALA_INFIX(   SUB,  "-",    Add)
IMPALA_INFIX(   MUL,  "*",    Mul)
IMPALA_INFIX(   DIV,  "/",    Mul)
IMPALA_INFIX(   REM,  "%",    Mul)
IMPALA_INFIX(    AS, "as",     As)

#undef IMPALA_INFIX

#ifndef IMPALA_KEY
#define IMPALA_KEY(tok, str)
#endif

IMPALA_KEY(DO,        "do")
IMPALA_KEY(ELSE,      "else")
IMPALA_KEY(ENUM,      "enum")
IMPALA_KEY(EXTERN,    "extern")
IMPALA_KEY(FALSE,     "false")
IMPALA_KEY(FN,        "fn")
IMPALA_KEY(FOR,       "for")
IMPALA_KEY(WITH,      "with")
IMPALA_KEY(IF,        "if")
IMPALA_KEY(MATCH,     "match")
IMPALA_KEY(IMPL,      "impl")
IMPALA_KEY(IN,        "in")
IMPALA_KEY(LET,       "let")
IMPALA_KEY(ASM,       "asm")
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
IMPALA_MISC(ARROW,        "->")
IMPALA_MISC(FAT_ARRROW,   "=>")
IMPALA_MISC(SEMICOLON,    ";")
IMPALA_MISC(COLON,        ":")
IMPALA_MISC(DOUBLE_COLON, ":")
IMPALA_MISC(COMMA,        ",")
IMPALA_MISC(DOTDOT,       "..")
IMPALA_MISC(RUN,          "@")
IMPALA_MISC(RUNRUN,       "@@")

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
IMPALA_LIT(f16, pf16)
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
IMPALA_TYPE(f16,  pf16)
IMPALA_TYPE(f32,  pf32)
IMPALA_TYPE(f64,  pf64)
IMPALA_TYPE(bool, bool)

#undef IMPALA_TYPE
