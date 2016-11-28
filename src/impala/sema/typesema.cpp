#include <sstream>

#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

class TypeSema {
public:
    TypeSema(bool nossa)
        : nossa_(nossa)
    {}

    // helpers

    bool nossa() const { return nossa_; }
    const Type* scalar_type(const Expr* expr) {
        if (auto simd_type = expr->type()->isa<SimdType>())
            return simd_type->elem_type();
        return expr->type();
    }

    // error handling

    template<typename... Args>
    void error_msg(const Expr* expr, const char* what, const Type* type, const char* fmt, Args... args) {
        std::ostringstream os;
        thorin::streamf(os, fmt, args...);
        error(expr, "mismatched types: expected % but found '%' as %", what, type, os.str());
    }

#define IMPALA_EXPECT(T, pred, what) \
    template<typename... Args> \
    void expect_##T(const Expr* expr, const char* fmt, Args... args) { \
        auto t = scalar_type(expr); \
        if (!t->isa<TypeError>() && t->is_known() && !(pred)) \
            error_msg(expr, what, t, fmt, args...); \
    }

    IMPALA_EXPECT(bool,        is_bool(t),                             "boolean type")
    IMPALA_EXPECT(int,         is_int(t),                              "integer type")
    IMPALA_EXPECT(int_or_bool, is_int(t)                || is_bool(t), "integer or boolean type")
    IMPALA_EXPECT(num,         is_int(t) || is_float(t),               "number type")
    IMPALA_EXPECT(num_or_bool, is_int(t) || is_float(t) || is_bool(t), "number or boolean type")
    IMPALA_EXPECT(ptr,         t->isa<PtrType>(),                      "pointer type")
    IMPALA_EXPECT(num_or_bool_or_ptr, is_int(t) || is_float(t) || is_bool(t) || t->isa<PtrType>(), "number or boolean or pointer type")

    template<typename... Args>
    void expect_lvalue(const Expr* expr, const char* fmt, Args... args) {
        std::ostringstream os;
        thorin::streamf(os, fmt, args...);
        if (!expr->is_lvalue())
            error(expr, "lvalue required for %", os.str());
    }

    void expect_known(const ValueDecl* value_decl) {
        if (!value_decl->type()->is_known()) {
            if (value_decl->symbol() == "return")
                error(value_decl, "cannot infer a return type, maybe you forgot to mark the function with '-> !'?");
            else
                error(value_decl, "cannot infer type for '%'", value_decl->symbol());
        }
    }

    void expect_type(const Type* expected, const Expr* expr, const char* context) {
        if (expected != expr->type() && expected->is_known() && expr->type()->is_known() && !expr->type()->isa<TypeError>())
            error(expr, "mismatched types: expected '%' but found '%' as %", expected, expr->type(), context);
    }

    // check wrappers

    const Var* check(const ASTTypeParam* ast_type_param) { ast_type_param->check(*this); return ast_type_param->var(); }
    void check(const ModContents* n) { n->check(*this); }
    const Type* check(const FieldDecl* n) { n->check(*this); return n->type(); }
    const Type* check(const LocalDecl* local) { local->check(*this); return local->type(); }
    const Type* check(const ASTType* ast_type) { ast_type->check(*this); return ast_type->type(); }
    void check(const Item* n) { n->check(*this); }
    const Type* check(const Expr* expr) { expr->check(*this); return expr->type(); }
    const Type* check(const Ptrn* p) { p->check(*this); return p->type(); }
    void check(const Stmt* n) { n->check(*this); }
    void check_call(const Expr* expr, ArrayRef<const Expr*> args);
    void check_call(const Expr* expr, const std::deque<AutoPtr<const Expr>>& args) {
        Array<const Expr*> array(args.begin(), args.end());
        check_call(expr, array);
    }

private:
    bool nossa_;

public:
    const BlockExprBase* cur_block_ = nullptr;
    const Fn* cur_fn_ = nullptr;
};

void type_analysis(const ModContents* mod, bool nossa) {
    TypeSema sema(nossa);
    sema.check(mod);
}

template<class T>
TokenKind token_kind(const T* expr) { return TokenKind(expr->kind()); }

template<class T>
const char* tok2str(const T* expr) { return Token::tok2str(token_kind(expr)); }

//------------------------------------------------------------------------------

/*
 * misc
 */

const Var* ASTTypeParam::check(TypeSema& sema) const {
    for (const auto& bound : bounds())
        sema.check(bound.get());

    return var();
}

void ASTTypeParamList::check_ast_type_params(TypeSema& sema) const {
    for (const auto& ast_type_param : ast_type_params())
        sema.check(ast_type_param.get());
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void ErrorASTType::check(TypeSema& ) const {}
void PrimASTType::check(TypeSema&) const {}
void PtrASTType::check(TypeSema& sema) const { sema.check(referenced_ast_type()); }
void IndefiniteArrayASTType::check(TypeSema& sema) const { sema.check(elem_ast_type()); }
void   DefiniteArrayASTType::check(TypeSema& sema) const { sema.check(elem_ast_type()); }

void SimdASTType::check(TypeSema& sema) const {
    if (!sema.check(elem_ast_type())->isa<PrimType>())
        error(this, "non primitive types forbidden in simd type");
}

void TupleASTType::check(TypeSema& sema) const {
    for (const auto& ast_type_arg : ast_type_args())
        sema.check(ast_type_arg.get());
}

void FnASTType::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    for (const auto& ast_type_arg : ast_type_args())
        sema.check(ast_type_arg.get());
}

void ASTTypeApp::check(TypeSema&) const {
    if (!decl() || !decl()->isa<TypeDecl>())
        error(identifier(), "'%' does not name a type", symbol());
}

void Typeof::check(TypeSema& sema) const { sema.check(expr()); }

//------------------------------------------------------------------------------

void LocalDecl::check(TypeSema& sema) const {
    fn_ = sema.cur_fn_;
    if (ast_type())
        sema.check(ast_type());
    sema.expect_known(this);
}

const Type* Fn::check_body(TypeSema& sema) const {
    sema.check(body());

    for (const auto& param : params()) {
        if (param->is_mut() && !param->is_written())
            warning(param.get(), "parameter '%' declared mutable but parameter is never written to", param->symbol());
    }

    if (!body()->type()->isa<NoRetType>())
        sema.expect_type(fn_type()->return_type(), body(), "return type");

    return body()->type();
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        sema.check(mod_contents());
}

void ModContents::check(TypeSema& sema) const {
    for (const auto& item : items())
        sema.check(item.get());
}

void ExternBlock::check(TypeSema& sema) const {
    if (!abi().empty()) {
        if (abi() != "\"C\"" && abi() != "\"device\"" && abi() != "\"thorin\"")
            error(this, "unknown extern specification");  // TODO: better location
    }

    for (const auto& fn_decl : fn_decls())
        sema.check(fn_decl.get());
}

void Typedef::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    sema.check(ast_type());
}

void EnumDecl::check(TypeSema&) const { /*TODO*/ }

void StructDecl::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    for (const auto& field_decl : field_decls())
        sema.check(field_decl.get());
}

void FieldDecl::check(TypeSema& sema) const { sema.check(ast_type()); }

void FnDecl::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    check_ast_type_params(sema);
    for (const auto& param : params())
        sema.check(param.get());

    if (body() != nullptr)
        check_body(sema);
}

void StaticItem::check(TypeSema& sema) const {
    if (init())
        sema.check(init());
    sema.expect_known(this);
}

void TraitDecl::check(TypeSema& sema) const {
    check_ast_type_params(sema);

    for (const auto& type_app : super_traits())
        sema.check(type_app.get());

    for (const auto& method : methods())
        sema.check(method.get());
}

void ImplItem::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    sema.check(this->ast_type());

    if (trait()) {
        if (trait()->isa<ASTTypeApp>()) {
            for (const auto& type_param : ast_type_params())
                sema.check(type_param.get());
        } else
            error(trait(), "expected trait instance");
    }
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::check(TypeSema&) const {}
void LiteralExpr::check(TypeSema&) const {}
void CharExpr::check(TypeSema&) const {}
void StrExpr::check(TypeSema&) const {}

void FnExpr::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    assert(ast_type_params().empty());

    for (size_t i = 0, e = num_params(); i != e; ++i)
        sema.check(param(i));

    assert(body() != nullptr);
    check_body(sema);
}

void PathExpr::check(TypeSema& sema) const {
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            // if local lies in an outer function go through memory to implement closure
            if (local->is_mut() && (sema.nossa() || local->fn() != sema.cur_fn_))
                local->take_address();
        }
    }
}

void PrefixExpr::check(TypeSema& sema) const {
    sema.check(rhs());

    switch (kind()) {
        case AND:
            sema.expect_lvalue(rhs(), "as unary '&' operand");
            rhs()->take_address();
            return;
        case TILDE:
            return;
        case MUL:
            sema.expect_ptr(rhs(), "unary '*'");
            return;
        case INC:
        case DEC:
            sema.expect_num(rhs(),    "prefix '%'", tok2str(this));
            sema.expect_lvalue(rhs(), "prefix '%'", tok2str(this));
            return;
        case ADD:
        case SUB:
            sema.expect_num(rhs(), "unary '%'", tok2str(this));
            return;
        case NOT:
            sema.expect_int_or_bool(rhs(), "unary '!'");
            return;
        case HLT:
        case RUN:
            if (!rhs()->isa<MapExpr>())
                error(this, "function call expected after partial evaluator command %", (TokenKind)kind());
            return;
        default:
            return;
    }

    THORIN_UNREACHABLE;
}

void InfixExpr::check(TypeSema& sema) const {
    sema.check(lhs());
    sema.check(rhs());

    if (lhs()->type() != rhs()->type() && !lhs()->type()->isa<TypeError>() && !rhs()->type()->isa<TypeError>()) {
        error(this, "both left-hand side and right-hand side of expression must agree on the same type");
        error(lhs(),  "left-hand side type is '%'", lhs()->type());
        error(rhs(), "right-hand side type is '%'", rhs()->type());
    }

    switch (kind()) {
        case EQ: case NE:
        case LT: case GT:
        case LE: case GE:
            sema.expect_num_or_bool_or_ptr(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_num_or_bool_or_ptr(rhs(), "right-hand side of binary '%'", tok2str(this));
            return;
        case ADD: case SUB:
        case MUL: case DIV: case REM:
            sema.expect_num(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_num(rhs(), "right-hand side of binary '%'", tok2str(this));
            return;
        case OROR: case ANDAND:
            sema.expect_bool(lhs(),  "left-hand side of logical '%'", tok2str(this));
            sema.expect_bool(rhs(), "right-hand side of logical '%'", tok2str(this));
            return;
        case SHL: case SHR:
            sema.expect_int(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_int(rhs(), "right-hand side of binary '%'", tok2str(this));
            return;
        case  OR: case AND: case XOR:
            sema.expect_int_or_bool(lhs(),  "left-hand side of bitwise '%'", tok2str(this));
            sema.expect_int_or_bool(rhs(), "right-hand side of bitwise '%'", tok2str(this));
            return;
        case ASGN:
            sema.expect_lvalue(lhs(), "assignment");
            return;
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
            sema.expect_num(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_num(rhs(), "right-hand side of binary '%'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '%'", tok2str(this));
            return;
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN:
            sema.expect_int_or_bool(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_int_or_bool(rhs(), "right-hand side of binary '%'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '%'", tok2str(this));
            return;
        case SHL_ASGN: case SHR_ASGN:
            sema.expect_int(lhs(),  "left-hand side of binary '%'", tok2str(this));
            sema.expect_int(rhs(), "right-hand side of binary '%'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '%'", tok2str(this));
            return;
        default: THORIN_UNREACHABLE;
    }
}

void PostfixExpr::check(TypeSema& sema) const {
    sema.check(lhs());
    sema.expect_num(lhs(),    "postfix '%'", tok2str(this));
    sema.expect_lvalue(lhs(), "postfix '%'", tok2str(this));
}

template <typename F, typename T>
bool symmetric(F f, T a, T b) { return f(a, b) || f(b, a); }

void CastExpr::check(TypeSema& sema) const {
    if (auto explicit_cast_expr = isa<ExplicitCastExpr>())
        sema.check(explicit_cast_expr->ast_type());

    auto src_type = sema.check(lhs());
    auto dst_type = type();

    // TODO be consistent: dst is first argument, src ist second argument
    auto ptr_to_ptr     = [&] (const Type* a, const Type* b) { return a->isa<PtrType>() && b->isa<PtrType>(); };
    auto int_to_int     = [&] (const Type* a, const Type* b) { return is_int(a)         && is_int(b);         };
    auto float_to_float = [&] (const Type* a, const Type* b) { return is_float(a)       && is_float(b);       };
    auto int_to_ptr     = [&] (const Type* a, const Type* b) { return is_int(a)         && b->isa<PtrType>(); };
    auto int_to_float   = [&] (const Type* a, const Type* b) { return is_int(a)         && is_float(b);       };
    auto int_to_bool    = [&] (const Type* a, const Type* b) { return is_int(a)         && is_bool(b);        };
    auto float_to_bool  = [&] (const Type* a, const Type* b) { return is_float(a)       && is_bool(b);        };

    bool valid_cast =
        ptr_to_ptr(src_type, dst_type) ||
        float_to_float(src_type, dst_type) ||
        int_to_int(src_type, dst_type) ||
        symmetric(int_to_ptr, src_type, dst_type) ||
        symmetric(int_to_float, src_type, dst_type) ||
        symmetric(int_to_bool, src_type, dst_type) ||
        symmetric(float_to_bool, src_type, dst_type);

    if (!valid_cast && !is_subtype(dst_type, src_type))
        error(this, "invalid source and destination types for cast operator, got '%' and '%'", src_type, dst_type);
}

void TupleExpr::check(TypeSema& sema) const {
    for (const auto& arg : args())
        sema.check(arg.get());
}

void RepeatedDefiniteArrayExpr::check(TypeSema& sema) const { sema.check(value()); }

void IndefiniteArrayExpr::check(TypeSema& sema) const {
    sema.check(dim());
    sema.expect_int(dim(), "dimensions in indefinite array expression");
    sema.check(elem_ast_type());
}

void DefiniteArrayExpr::check(TypeSema& sema) const {
    const Type* elem_type = nullptr;
    if (auto definite_array_type = type()->isa<DefiniteArrayType>())
        elem_type = definite_array_type->elem_type();

    for (const auto& arg : args()) {
        sema.check(arg.get());
        if (elem_type)
            sema.expect_type(elem_type, arg.get(), "element of definite array expression");
    }
}

void SimdExpr::check(TypeSema& sema) const {
    const Type* elem_type = nullptr;
    if (auto simd_type = type()->isa<SimdType>())
        elem_type = simd_type->elem_type();

    for (const auto& arg : args()) {
        sema.check(arg.get());
        if (elem_type)
            sema.expect_type(elem_type, arg.get(), "element of simd expression");
    }
}

void StructExpr::check(TypeSema& sema) const {
    auto type = sema.check(ast_type_app());
    if (auto struct_type = type->isa<StructType>()) {
        auto struct_decl = struct_type->struct_decl();
        thorin::HashSet<const FieldDecl*> done;
        for (const auto& elem : elems()) {
            sema.check(elem.expr());

            if (auto field_decl = struct_decl->field_decl(elem.symbol())) {
                elem.field_decl_ = field_decl;

                if (!thorin::visit(done, field_decl))
                    sema.expect_type(struct_type->op(field_decl->index()), elem.expr(), "initialization type for field");
                else
                    error(elem.expr(), "field '%' specified more than once", elem.symbol());
            } else
                error(elem.expr(), "structure '%' has no field named '%'", struct_decl->symbol(), elem.symbol());
        }

        if (done.size() != struct_decl->field_table().size()) {
            for (const auto& p : struct_decl->field_table()) {
                if (!done.contains(p.second))
                    error(this, "missing field '%'", p.first);
            }
        }
    } else if (type->is_known() && !type->isa<TypeError>())
        error(ast_type_app(), "'%' is not a structure", type);
}

void FieldExpr::check(TypeSema& sema) const {
    auto type = sema.check(lhs());
    if (auto struct_type = type->isa<StructType>()) {
        auto struct_decl = struct_type->struct_decl();
        if (auto field_decl = struct_decl->field_decl(symbol()))
            field_decl_ = field_decl;
        else
            error(lhs(), "attempted access of field '%' on type '%', but no field with that name was found", symbol(), type);
    } else if (!type->isa<TypeError>())
        error(lhs(), "request for field '%' in something not a structure", symbol());
}

void TypeAppExpr::check(TypeSema& /*sema*/) const {
}

void MapExpr::check(TypeSema& sema) const {
    auto ltype = sema.check(lhs());
    for (const auto& arg : args())
        sema.check(arg.get());

    if (ltype->isa<FnType>()) {
        sema.check_call(lhs(), args());
    } else if (ltype->isa<ArrayType>()) {
        if (num_args() == 1)
            sema.expect_int(arg(0), "for array subscript");
        else
            error(this, "too many array subscripts");
    } else if (ltype->isa<TupleType>()) {
        if (num_args() == 1) {
            sema.expect_int(arg(0), "for tuple subscript");
            if (!arg(0)->isa<LiteralExpr>())
                error(this, "require literal as tuple subscript");
        } else
            error(this, "too many tuple subscripts");
    } else if(ltype->isa<SimdType>()) {
        if (num_args() == 1)
            sema.expect_int(arg(0), "require integer as vector subscript");
        else
            error(this, "too many simd vector subscripts");
    } else
        error(this, "incorrect type for map expression");
}

void TypeSema::check_call(const Expr* expr, ArrayRef<const Expr*> args) {
    auto fn_type = expr->type()->as<FnType>();

    if (fn_type->num_ops() == args.size() || fn_type->num_ops() == args.size() + 1) {
        for (size_t i = 0; i < args.size(); i++)
            expect_type(fn_type->op(i), args[i], "argument type");
    } else
        error(expr, "incorrect number of arguments in function application: got %, expected %", args.size(), fn_type->num_ops() - 1);
}

void BlockExprBase::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_block_, this);
    for (const auto& stmt : stmts())
        sema.check(stmt);

    sema.check(expr());

    for (const auto& local : locals_) {
        if (local->is_mut() && !local->is_written())
            warning(local, "variable '%' declared mutable but variable is never written to", local->symbol());
    }
}

void IfExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.expect_bool(cond(), "if condition");
    auto then_type = sema.check(then_expr());
    auto else_type = sema.check(else_expr());

    if (then_type != else_type && !then_type->isa<NoRetType>() && !else_type->isa<NoRetType>())
        sema.expect_type(then_type, else_expr(), "else branch type");
}

void WhileExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body());
}

void ForExpr::check(TypeSema& sema) const {
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();

    if (auto map = forexpr->isa<MapExpr>()) {
        auto ltype = sema.check(map->lhs());
        for (const auto& arg : map->args())
            sema.check(arg);
        sema.check(fn_expr());

        if (auto fn_for = ltype->isa<FnType>()) {
            if (fn_for->num_ops() != 0) {
                if (fn_for->ops().back()->isa<FnType>()) {
                    // copy over args and check call
                    Array<const Expr*> args(map->args().size()+1);
                    *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
                    sema.check_call(map->lhs(), args);
                    return;
                }
            }
        }
    }

    error(expr(), "the looping expression does not support the 'for' protocol");
}

//------------------------------------------------------------------------------

/*
 * patterns
 */

void TuplePtrn::check(TypeSema& sema) const {
    for (const auto& elem : elems()) {
        sema.check(elem);
    }
}

void IdPtrn::check(TypeSema& sema) const {
    sema.cur_block_->add_local(local());
    sema.check(local());
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(TypeSema& sema) const {
    if (sema.check(expr())->isa<NoRetType>())
        error(expr(), "expression does not return; subsequent statements are unreachable");
    if (!expr()->has_side_effect())
        warning(expr(), "statement with no effect");
}

void ItemStmt::check(TypeSema& sema) const {
    sema.check(item());
}

void LetStmt::check(TypeSema& sema) const {
    auto type = sema.check(ptrn());
    if (init()) {
        auto init_type = sema.check(init());
        if (!is_subtype(init_type, type))
            error(this, "let pattern type does not match initializer type, got '%' and '%'", type, init_type);
    } else {
        auto id_ptrn = ptrn()->isa<IdPtrn>();
        // Ptrns and non-mutable variables need an initialization
        if (!id_ptrn)
            error(this, "let pattern lacks initialization");
        else if (!id_ptrn->local()->is_mut())
            error(this, "non-mutable let statement lacks initialization");
    }
}

void check_correct_asm_type(const Type* t, const Expr *expr) {
    if (!t->isa<PrimType>() && !t->isa<PtrType>() && !t->isa<SimdType>())
        error(expr, "asm operand must have primitive, pointer or SIMD type, but it is %");
}

void AsmStmt::check(TypeSema& sema) const {
    for (const auto& output : outputs()) {
        if (!output.expr()->is_lvalue())
            error(output.expr(), "output expression of an asm statement must be an lvalue");
        check_correct_asm_type(sema.check(output.expr()), output.expr());
    }

    for (const auto& input : inputs())
        check_correct_asm_type(sema.check(input.expr()), input.expr());

    for (const auto& option : options()) {
        if (option != "volatile" && option != "alignstack" && option != "intel")
            error(this, "unsupported inline assembly option '%', only 'volatile', 'alignstack' and 'intel' supported", option);
    }
}

//------------------------------------------------------------------------------

}
