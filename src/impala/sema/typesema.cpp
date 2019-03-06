#include <sstream>

#include "impala/ast.h"
#include "impala/impala.h"

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
        auto result = unpack_ref_type(expr->type());
        if (auto simd_type = result->isa<SimdType>())
            result = simd_type->elem_type();
        return result;
    }

    // error handling

    template<typename... Args>
    void error_msg(const Expr* expr, const char* what, const Type* type, const char* fmt, Args... args) {
        std::ostringstream os;
        thorin::streamf(os, fmt, args...);
        error(expr, "mismatched types: expected {} but found '{}' at {}", what, type, os.str());
    }

#define IMPALA_EXPECT(T, pred, what) \
    template<typename... Args> \
    void expect_##T(const Expr* expr, const char* fmt, Args... args) { \
        auto t = scalar_type(expr); \
        if (!t->isa<TypeError>() && t->is_known() && !(pred)) \
            error_msg(expr, what, t, fmt, args...); \
    }

    IMPALA_EXPECT(unit,        is_unit(t),                             "unit type")
    IMPALA_EXPECT(bool,        is_bool(t),                             "boolean type")
    IMPALA_EXPECT(int,         is_int(t),                              "integer type")
    IMPALA_EXPECT(int_or_bool, is_int(t)                || is_bool(t), "integer or boolean type")
    IMPALA_EXPECT(num,         is_int(t) || is_float(t),               "number type")
    IMPALA_EXPECT(num_or_bool, is_int(t) || is_float(t) || is_bool(t), "number or boolean type")
    IMPALA_EXPECT(ptr,         t->isa<PtrType>(),                      "pointer type")
    IMPALA_EXPECT(num_or_bool_or_ptr, is_int(t) || is_float(t) || is_bool(t) || t->isa<PtrType>(), "number or boolean or pointer type")
    IMPALA_EXPECT(fn,          t->isa<FnType>(),                       "function type")

    template<typename... Args>
    const Type* expect_lvalue(const Expr* expr, const char* fmt, Args... args) {
        std::ostringstream os;
        thorin::streamf(os, fmt, args...);
        if (auto ref = is_lvalue(expr->type()))
            return ref->pointee();
        error(expr, "lvalue required for {}", os.str());
        return expr->type();
    }

    void expect_known(const Decl* value_decl) {
        if (!value_decl->type()->is_known()) {
            if (value_decl->symbol() == "return")
                error(value_decl, "cannot infer a return type, maybe you forgot to mark the function with '-> !'?");
            else
                error(value_decl, "cannot infer type for '{}'", value_decl->symbol());
        }
    }

    void expect_type(const Type* expected, const Typeable* node, const char* context) {
        if (expected == node->type()) {
            // note: for type inference errors, it is unclear which one of 'src' and 'dst' is the correct type
            if (auto infer_error = expected->isa<InferError>())
                error(node, "incompatible types: '{}' and '{}' found as {}", infer_error->src(), infer_error->dst(), context);
        } else {
            if (expected->is_known() && node->type()->is_known() && !node->type()->isa<TypeError>())
                error(node, "mismatched types: expected '{}' but found '{}' as {}", expected, node->type(), context);
        }
    }

    void no_indefinite_array(const ASTNode* n, const Type* type, const char* context) {
        if (type->isa<IndefiniteArrayType>())
            error(n, "indefinite array '{}' not allowed as {} because its size is statically unknown; use a definite array or a pointer to an indefinite array instead", type, context);
    }

    // check wrappers

    const Var* check(const ASTTypeParam* ast_type_param) { ast_type_param->check(*this); return ast_type_param->var(); }
    void check(const Module* module) { module->check(*this); }
    const Type* check(const FieldDecl* n) { n->check(*this); return n->type(); }
    const Type* check(const OptionDecl* n) { n->check(*this); return n->type(); }
    const Type* check(const LocalDecl* local) { local->check(*this); return local->type(); }
    const Type* check(const ASTType* ast_type) { ast_type->check(*this); return ast_type->type(); }
    void check(const Item* n) { n->check(*this); }
    const Type* check(const Expr* expr) { expr->check(*this); return expr->type(); }
    const Type* check(const Ptrn* p) { p->check(*this); return p->type(); }
    void check(const Stmt* n) { n->check(*this); }
    void check_call(const Expr* expr, ArrayRef<const Expr*> args);
    void check_call(const Expr* expr, const Exprs& args) {
        Array<const Expr*> array(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            array[i] = args[i].get();
        check_call(expr, array);
    }

private:
    bool nossa_;

public:
    const BlockExpr* cur_block_ = nullptr;
    const Fn* cur_fn_ = nullptr;
};

void type_analysis(const Module* module, bool nossa) {
    TypeSema sema(nossa);
    sema.check(module);
}

template<class T>
TokenTag token_tag(const T* expr) { return TokenTag(expr->tag()); }

template<class T>
const char* tok2str(const T* expr) { return Token::tok2str(token_tag(expr)); }

//------------------------------------------------------------------------------

/*
 * misc
 */

const Var* ASTTypeParam::check(TypeSema& sema) const {
    for (auto&& bound : bounds())
        sema.check(bound.get());

    return var();
}

void ASTTypeParamList::check_ast_type_params(TypeSema& sema) const {
    for (auto&& ast_type_param : ast_type_params())
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
    for (auto&& ast_type_arg : ast_type_args()) {
        sema.check(ast_type_arg.get());
        sema.no_indefinite_array(ast_type_arg.get(), ast_type_arg->type(), "element type in a tuple");
    }
}

void FnASTType::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    for (auto&& ast_type_arg : ast_type_args()) {
        sema.check(ast_type_arg.get());
        sema.no_indefinite_array(ast_type_arg.get(), ast_type_arg->type(), "element type in a function type");
    }
}

void ASTTypeApp::check(TypeSema& sema) const {
    path()->check(sema);
    if (!decl() || !decl()->is_type_decl())
        error(identifier(), "'{}' does not name a type", symbol());
}

void Typeof::check(TypeSema& sema) const { sema.check(expr()); }

//------------------------------------------------------------------------------

void LocalDecl::check(TypeSema& sema) const {
    fn_ = sema.cur_fn_;
    if (ast_type())
        sema.check(ast_type());
    sema.expect_known(this);
    sema.no_indefinite_array(ast_type() ? ast_type()->as<ASTNode>() : identifier()->as<ASTNode>(), type(),
            isa<Param>() ? "parameter type" : "type for a local variable");
}

const Type* Fn::check_body(TypeSema& sema) const {
    sema.check(body());

    for (auto&& param : params()) {
        if (param->is_mut() && !param->is_written())
            warning(param.get(), "parameter '{}' declared mutable but parameter is never written to", param->symbol());
    }

    if (!body()->type()->isa<NoRetType>())
        sema.expect_type(fn_type()->return_type(), body(), "return type");

    return body()->type();
}

void Path::check(TypeSema&) const {
    auto last_type = elem(0)->type();
    for (size_t i = 1, e = num_elems(); i != e; ++i) {
        auto cur_type = elem(i)->type();
        if (cur_type->isa<TypeError>()) {
            error(this, "'{}' is not a member of '{}'", elem(i)->symbol(), last_type);
            break;
        }
        last_type = cur_type;
    }
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModuleDecl::check(TypeSema&) const {
}

void Module::check(TypeSema& sema) const {
    for (auto&& item : items())
        sema.check(item.get());
}

void ExternBlock::check(TypeSema& sema) const {
    if (!abi().empty()) {
        if (abi() != "\"C\"" && abi() != "\"device\"" && abi() != "\"thorin\"")
            error(this, "unknown extern specification");  // TODO: better location
    }

    for (auto&& fn_decl : fn_decls())
        sema.check(fn_decl.get());
}

void Typedef::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    sema.check(ast_type());
}

void EnumDecl::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    for (auto&& option : option_decls())
        sema.check(option.get());
}

void OptionDecl::check(TypeSema& sema) const {
    for (auto&& arg : args()) sema.check(arg.get());
}

void StructDecl::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    for (auto&& field_decl : field_decls()) {
        sema.check(field_decl.get());
        sema.no_indefinite_array(field_decl.get(), field_decl->type(), "type for a struct field");
    }
}

void FieldDecl::check(TypeSema& sema) const { sema.check(ast_type()); }

void FnDecl::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    check_ast_type_params(sema);
    for (auto&& param : params())
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

    for (auto&& type_app : super_traits())
        sema.check(type_app.get());

    for (auto&& method : methods())
        sema.check(method.get());
}

void ImplItem::check(TypeSema& sema) const {
    check_ast_type_params(sema);
    sema.check(this->ast_type());

    if (trait()) {
        if (trait()->isa<ASTTypeApp>()) {
            for (auto&& type_param : ast_type_params())
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
    path()->check(sema);
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            // if local lies in an outer function go through memory to implement closure
            if (local->is_mut() && (sema.nossa() || local->fn() != sema.cur_fn_))
                local->take_address();
        }
    } else
        error(this, "expected value but found '{}'", path());
}

void PrefixExpr::check(TypeSema& sema) const {
    sema.check(rhs());

    switch (tag()) {
        case AND:
            rhs()->take_address();
            return;
        case MUT:
            rhs()->write();
            rhs()->take_address();
            sema.expect_lvalue(rhs(), "operand of '&mut'");
            return;
        case TILDE:
            return;
        case MUL:
            sema.expect_ptr(rhs(), "operand of unary '*'");
            return;
        case INC: case DEC: {
            rhs()->write();
            sema.expect_lvalue(rhs(), "operand of prefix '{}'", tok2str(this));
            sema.expect_num(rhs(),    "operand of prefix '{}'", tok2str(this));
            return;
        }
        case ADD: case SUB:
            sema.expect_num(rhs(), "operand of unary '{}'", tok2str(this));
            return;
        case NOT:
            sema.expect_int_or_bool(rhs(), "operand of '!'");
            return;
        case HLT:
        case KNOWN:
            return;
        case RUNRUN:
            sema.expect_fn(rhs(), "operand of '{}'", tok2str(this));
            return;
        case OR: case OROR: case RUN: // Lambda
            THORIN_UNREACHABLE;
    }

    THORIN_UNREACHABLE;
}

void InfixExpr::check(TypeSema& sema) const {
    sema.check(lhs());
    sema.check(rhs());

    auto match_type = [&](const Type* ltype, const Type* rtype) {
        if (ltype != rtype && !ltype->isa<TypeError>() && !rtype->isa<TypeError>()) {
            error(this, "both left-hand side and right-hand side of binary '{}' must agree on the same type", tok2str(this));
            error(lhs(),  "left-hand side type is '{}'", ltype);
            error(rhs(), "right-hand side type is '{}'", rtype);
        }
    };

    auto match_subtype = [&](const Type* ltype, const Type* rtype) {
        ltype = unpack_ref_type(ltype);
        if (!is_subtype(ltype, rtype) && !ltype->isa<TypeError>() && !rtype->isa<TypeError>()) {
            error(this, "right-hand side of binary '{}' must be a subtype of its left-hand side", tok2str(this));
            error(lhs(),  "left-hand side type is '{}'", ltype);
            error(rhs(), "right-hand side type is '{}'", rtype);
        }
    };

    switch (tag()) {
        case EQ: case NE:
        case LT: case GT:
        case LE: case GE:
            match_type(lhs()->type(), rhs()->type());
            sema.expect_num_or_bool_or_ptr(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_num_or_bool_or_ptr(rhs(), "right-hand side of binary '{}'", tok2str(this));
            return;
        case ADD: case SUB:
        case MUL: case DIV: case REM:
            match_type(lhs()->type(), rhs()->type());
            sema.expect_num(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_num(rhs(), "right-hand side of binary '{}'", tok2str(this));
            return;
        case OROR: case ANDAND:
            match_type(lhs()->type(), rhs()->type());
            sema.expect_bool(lhs(),  "left-hand side of logical '{}'", tok2str(this));
            sema.expect_bool(rhs(), "right-hand side of logical '{}'", tok2str(this));
            return;
        case SHL: case SHR:
            match_type(lhs()->type(), rhs()->type());
            sema.expect_int(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_int(rhs(), "right-hand side of binary '{}'", tok2str(this));
            return;
        case  OR: case AND: case XOR:
            match_type(lhs()->type(), rhs()->type());
            sema.expect_int_or_bool(lhs(),  "left-hand side of bitwise '{}'", tok2str(this));
            sema.expect_int_or_bool(rhs(), "right-hand side of bitwise '{}'", tok2str(this));
            return;
        case ASGN: {
            lhs()->write();
            match_subtype(lhs()->type(), rhs()->type());
            sema.expect_lvalue(lhs(), "assignment");
            return;
        }
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
            lhs()->write();
            match_subtype(lhs()->type(), rhs()->type());
            sema.expect_num(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_num(rhs(), "right-hand side of binary '{}'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '{}'", tok2str(this));
            return;
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN:
            lhs()->write();
            match_subtype(lhs()->type(), rhs()->type());
            sema.expect_int_or_bool(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_int_or_bool(rhs(), "right-hand side of binary '{}'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '{}'", tok2str(this));
            return;
        case SHL_ASGN: case SHR_ASGN:
            lhs()->write();
            match_subtype(lhs()->type(), rhs()->type());
            sema.expect_int(lhs(),  "left-hand side of binary '{}'", tok2str(this));
            sema.expect_int(rhs(), "right-hand side of binary '{}'", tok2str(this));
            sema.expect_lvalue(lhs(), "assignment '{}'", tok2str(this));
            return;
        default: THORIN_UNREACHABLE;
    }
}

void PostfixExpr::check(TypeSema& sema) const {
    lhs()->write();
    sema.check(lhs());
    sema.expect_num(lhs(),    "postfix '{}'", tok2str(this));
    sema.expect_lvalue(lhs(), "postfix '{}'", tok2str(this));
}

template <typename F, typename T>
bool symmetric(F f, T a, T b) { return f(a, b) || f(b, a); }

void CastExpr::check(TypeSema& sema) const {
    auto src_type = sema.check(src());
    auto dst_type = type();

    if (dst_type->isa<BorrowedPtrType>() && dst_type->as<BorrowedPtrType>()->is_mut())
        src()->write();

    // TODO be consistent: dst is first argument, src ist second argument
    auto ptr_to_ptr     = [&] (const Type* a, const Type* b) { return a->isa<PtrType>() && b->isa<PtrType>(); };
    auto int_to_int     = [&] (const Type* a, const Type* b) { return is_int(a)         && is_int(b);         };
    auto float_to_float = [&] (const Type* a, const Type* b) { return is_float(a)       && is_float(b);       };
    auto int_to_ptr     = [&] (const Type* a, const Type* b) { return is_int(a)         && b->isa<PtrType>(); };
    auto int_to_float   = [&] (const Type* a, const Type* b) { return is_int(a)         && is_float(b);       };
    auto int_to_bool    = [&] (const Type* a, const Type* b) { return is_int(a)         && is_bool(b);        };
    auto float_to_bool  = [&] (const Type* a, const Type* b) { return is_float(a)       && is_bool(b);        };

    bool valid_cast = ptr_to_ptr(src_type, dst_type)
        || float_to_float(src_type, dst_type)
        || int_to_int(src_type, dst_type)
        || symmetric(int_to_ptr, src_type, dst_type)
        || symmetric(int_to_float, src_type, dst_type)
        || symmetric(int_to_bool, src_type, dst_type)
        || symmetric(float_to_bool, src_type, dst_type);

    if (src_type->is_known() && dst_type->is_known() && !valid_cast && !is_subtype(dst_type, src_type))
        error(this, "invalid source and destination types for cast operator, got '{}' and '{}'", src_type, dst_type);
}

void ExplicitCastExpr::check(TypeSema& sema) const {
    sema.check(ast_type());
    CastExpr::check(sema);
}

void RValueExpr::check(TypeSema& sema) const {
    sema.check(src());
}

void TupleExpr::check(TypeSema& sema) const {
    for (auto&& arg : args()) {
        sema.check(arg.get());
        sema.no_indefinite_array(arg.get(), arg->type(), "type for an element in a tuple expression");
    }
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

    for (auto&& arg : args()) {
        sema.check(arg.get());
        if (elem_type)
            sema.expect_type(elem_type, arg.get(), "element of definite array expression");
    }
}

void SimdExpr::check(TypeSema& sema) const {
    const Type* elem_type = nullptr;
    if (auto simd_type = type()->isa<SimdType>())
        elem_type = simd_type->elem_type();

    for (auto&& arg : args()) {
        sema.check(arg.get());
        if (elem_type)
            sema.expect_type(elem_type, arg.get(), "element of simd expression");
    }
}

void StructExpr::check(TypeSema& sema) const {
    auto type = sema.check(ast_type_app());
    if (auto struct_type = type->isa<StructType>()) {
        auto struct_decl = struct_type->struct_decl();
        thorin::GIDSet<const FieldDecl*> done;
        for (auto&& elem : elems()) {
            sema.check(elem->expr());

            if (auto field_decl = elem->field_decl()) {
                if (!thorin::visit(done, field_decl))
                    sema.expect_type(struct_type->op(field_decl->index()), elem->expr(), "initialization type for field");
                else
                    error(elem->expr(), "field '{}' specified more than once", elem->symbol());
            } else
                error(elem->expr(), "structure '{}' has no field named '{}'", struct_decl->symbol(), elem->symbol());
        }

        if (done.size() != struct_decl->field_table().size()) {
            for (auto&& p : struct_decl->field_table()) {
                if (!done.contains(p.second))
                    error(this, "missing field '{}'", p.first);
            }
        }
    } else if (type->is_known() && !type->isa<TypeError>())
        error(ast_type_app(), "'{}' is not a structure", type);
}

void FieldExpr::check(TypeSema& sema) const {
    auto type = unpack_ref_type(sema.check(lhs()));

    if (auto struct_type = type->isa<StructType>()) {
        auto struct_decl = struct_type->struct_decl();
        if (auto field_decl = struct_decl->field_decl(symbol()))
            field_decl_ = field_decl;
        else
            error(lhs(), "attempted access of field '{}' on type '{}', but no field with that name was found", symbol(), type);
    } else if (!type->isa<TypeError>())
        error(lhs(), "request for field '{}' in something not a structure", symbol());
}

void TypeAppExpr::check(TypeSema& /*sema*/) const {
}

void MapExpr::check(TypeSema& sema) const {
    auto ltype = unpack_ref_type(sema.check(lhs()));

    for (auto&& arg : args())
        sema.check(arg.get());

    if (ltype->isa<FnType>()) {
        if (!type()->is_known())
            error(this, "cannot infer type for function call");
        return sema.check_call(lhs(), args());
    }

    if (ltype->isa<ArrayType>()) {
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

    if (fn_type->num_params() == args.size() || fn_type->num_params() == args.size() + 1) {
        for (size_t i = 0; i < args.size(); i++)
            expect_type(fn_type->param(i), args[i], "argument type");
    } else
        error(expr, "incorrect number of arguments in function application: got {}, expected {}",
              args.size(), std::max(size_t(0), fn_type->num_params() - (fn_type->is_returning() ? 1 : 0)));
}

void BlockExpr::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_block_, this);
    for (auto&& stmt : stmts())
        sema.check(stmt.get());

    sema.check(expr());

    for (auto&& local : locals_) {
        if (local->is_mut() && !local->is_written())
            warning(local, "variable '{}' declared mutable but variable is never written to", local->symbol());
    }
}

void IfExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.expect_bool(cond(), "if-condition");
    auto then_type = sema.check(then_expr());
    auto else_type = sema.check(else_expr());

    if (!is_no_ret_or_type_error(then_type) && !is_no_ret_or_type_error(else_type) && then_type != else_type)
        sema.expect_type(then_type, else_expr(), "else branch type");
}

inline bool is_match_complete(const MatchExpr* match) {
    if (auto enum_type = match->expr()->type()->isa<EnumType>()) {
        // match is complete if every option is covered by an irrefutable pattern
        auto enum_decl = enum_type->enum_decl();
        Array<bool> covered(enum_decl->num_option_decls(), false);
        size_t num_covered = 0;

        for (size_t i = 0, e = match->num_arms(); i != e; ++i) {
            auto enum_ptrn = match->arm(i)->ptrn()->isa<EnumPtrn>();
            if (!enum_ptrn) continue;
            auto option_decl = enum_ptrn->path()->decl()->isa<OptionDecl>();
            if (!option_decl || option_decl->enum_decl() != enum_decl) continue;

            bool refutable = false;
            for (auto& arg : enum_ptrn->args()) refutable |= arg->is_refutable();
            if (refutable) continue;

            num_covered += covered[option_decl->index()] ? 0 : 1;
            covered[option_decl->index()] = true;
        }

        if (num_covered == enum_decl->num_option_decls()) return true;
    }

    // match is complete if there is an irrefutable pattern
    for (size_t i = 0, e = match->num_arms(); i != e; ++i) {
        if (!match->arm(i)->ptrn()->is_refutable()) return true;
    }

    return false;
}

void MatchExpr::check(TypeSema& sema) const {
    auto expr_type = sema.check(expr());
    auto arg_type  = num_arms() > 0 ? sema.check(arm(0)->expr()) : nullptr;
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        sema.check(arm(i)->ptrn());
        sema.check(arm(i)->expr());

        sema.expect_type(expr_type, arm(i)->ptrn(), "pattern type");
        if (!is_no_ret_or_type_error(arm(i)->expr()->type()))
            sema.expect_type(arg_type,  arm(i)->expr(), "matched expression type");
        if (!arm(i)->ptrn()->is_refutable() && i < e - 1)
            warning(arm(i)->ptrn(), "pattern is always true, subsequent patterns will not be executed");
    }
    if (!is_match_complete(this))
        error(this, "missing default case for match expression");
}

void WhileExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.expect_bool(cond(), "while-condition");
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body());

    if (!is_no_ret_or_type_error(body()->type()))
        sema.expect_unit(body(), "body type in a while-expression");
}

void ForExpr::check(TypeSema& sema) const {
    auto forexpr = expr();

    if (auto map = forexpr->isa<MapExpr>()) {
        auto ltype = sema.check(map->lhs());
        for (auto&& arg : map->args())
            sema.check(arg.get());
        sema.check(fn_expr());

        if (auto fn_for = ltype->isa<FnType>()) {
            if (fn_for->num_params() != 0) {
                if (fn_for->last_param()->isa<FnType>()) {
                    // TODO remove copy & paste code
                    // copy over args and check call --
                    Array<const Expr*> args(map->num_args() + 1);
                    for (size_t i = 0, e = map->num_args(); i != e; ++i)
                        args[i] = map->arg(i);
                    args.back() = fn_expr();
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
    for (auto&& elem : elems()) {
        sema.check(elem.get());
    }
}

void IdPtrn::check(TypeSema& sema) const {
    sema.cur_block_->add_local(local());
    sema.check(local());
}

void EnumPtrn::check(TypeSema& sema) const {
    for (auto&& arg : args())
        sema.check(arg.get());

    auto option_decl = path()->decl()->isa<OptionDecl>();
    if (option_decl) {
        if (option_decl->num_args() != num_args()) {
            error(this, "incorrect number of arguments for enumeration variant");
        } else {
            for (size_t i = 0, e = num_args(); i != e; ++i) {
                sema.expect_type(option_decl->arg(i)->type(), arg(i), "argument of enumeration variant");
            }
        }
    }
}

void LiteralPtrn::check(TypeSema& sema) const {
    sema.check(literal());
    if (has_minus())
        sema.expect_num(literal(), "literal pattern");
}

void CharPtrn::check(TypeSema& sema) const {
    sema.check(chr());
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

    if (ptrn()->is_refutable())
        error(this, "refutable pattern in let statement");

    if (!type->is_known())
        error(this, "cannot infer type for let statement");
    else if (init()) {
        auto init_type = sema.check(init());
        if (!init_type->is_known())
            error(this, "cannot infer type for let initializer");
        else if (!is_subtype(init_type, type))
            error(this, "let pattern type does not match initializer type, got '{}' and '{}'", type, init_type);
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
        error(expr, "asm operand must have primitive, pointer or SIMD type, got '{}'", t);
}

void AsmStmt::check(TypeSema& sema) const {
    for (auto&& output : outputs()) {
        auto type = sema.check(output->expr());

        if (auto ref = is_lvalue(type))
            type = ref->pointee();
        else
            error(output->expr(), "output expression of an asm statement must be an lvalue");

        output->expr()->write();
        check_correct_asm_type(type, output->expr());
    }

    for (auto&& input : inputs())
        check_correct_asm_type(sema.check(input->expr()), input->expr());

    for (auto&& option : options()) {
        if (option != "volatile" && option != "alignstack" && option != "intel")
            error(this, "unsupported inline assembly option '{}', only 'volatile', 'alignstack' and 'intel' supported", option);
    }
}

//------------------------------------------------------------------------------

}
