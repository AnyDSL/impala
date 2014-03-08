#include <iostream>

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    void push_impl(const Impl* i) { impls_.push_back(i); }
    void check_impls() {
        while (!impls_.empty()) {
            const Impl* i = impls_.back();
            impls_.pop_back();
            i->check(*this);
        }
    }
    void expect_num(const Expr* exp);
    Type match_types(const Expr* pos, Type t1, Type t2);
    void expect_type(const Expr* found, Type expected, std::string typetype);
    Type create_return_type(const ASTNode* node, Type ret_func);

private:
    bool nossa_;
    std::vector<const Impl*> impls_;
};

//------------------------------------------------------------------------------

void TypeSema::expect_num(const Expr* exp) {
    Type t = exp->type();

    if (t == type_error())
        return;

    if ((t != type_int()) && (t != type_int8()) && (t != type_int16()) && (t != type_int32()) &&
            (t != type_int64()) && (t != type_float()) && (t != type_double()))
        error(exp) << "expected number type but found " << t << "\n";
}

Type TypeSema::match_types(const Expr* pos, Type t1, Type t2) {
    if (t1 == type_error() || t2 == type_error())
        return type_error();

    if (t1 == t2) {
        return t1;
    } else {
        error(pos) << "types do not match: " << t1 << " != " << t2 << "\n";
        return type_error();
    }
}

void TypeSema::expect_type(const Expr* found, Type expected, std::string typetype) {
    if (found->type() == type_error() || expected == type_error())
        return;
    if (found->type() != expected)
        error(found) << "wrong " << typetype << " type; expected " << expected << " but found " << found->type() << "\n";
}

Type TypeSema::create_return_type(const ASTNode* node, Type ret_func) {
    if (ret_func.isa<FnType>()) {
        if (ret_func->size() == 1) {
            return ret_func->elem(0);
        } else {
            std::vector<Type> ret_types;
            for (auto t : ret_func->elems())
                ret_types.push_back(t);
            return tupletype(ret_types);
        }
    } else {
        error(node) << "last argument is not a continuation function\n";
        return type_error();
    }
}

//------------------------------------------------------------------------------

/*
 * ASTType::to_type
 */

void ParametricASTType::check_type_params(TypeSema& sema) const {
    // check bounds
    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>()) {
                tp->type_var()->add_bound(trait_inst->to_trait(sema));
            } else {
                sema.error(tp) << "bounds must be trait instances, not types\n";
            }
        }
    }
}

Type ErrorASTType::to_type(TypeSema& sema) const { return sema.type_error(); }

Type PrimASTType::to_type(TypeSema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.primtype(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::to_type(TypeSema& sema) const {
    return Type(); // FEATURE
}

Type IndefiniteArrayASTType::to_type(TypeSema& sema) const {
    return Type(); // FEATURE
}

Type DefiniteArrayASTType::to_type(TypeSema& sema) const {
    return Type(); // FEATURE
}

Type TupleASTType::to_type(TypeSema& sema) const {
    std::vector<Type> elems;
    for (auto e : this->elems())
        elems.push_back(e->to_type(sema));

    return sema.tupletype(elems);
}

Type ASTTypeApp::to_type(TypeSema& sema) const {
    if (type_decl())
        return type_decl()->type_var();
    else 
        return sema.type_error();
}

Type FnASTType::to_type(TypeSema& sema) const {
    check_type_params(sema);

    std::vector<Type> params;
    for (auto elem : elems())
        params.push_back(elem->to_type(sema));

    FnType fntype = sema.fntype(params);
    for (auto type_param : type_params())
        fntype->add_bound_var(type_param->type_var());

    return fntype;
}

Trait ASTTypeApp::to_trait(TypeSema& sema) const {
#if 0
    if (auto decl = sema.lookup(this, symbol())) {
        if (auto trait_decl = decl->isa<TraitDecl>()) {
            Trait trait = trait_decl->calc_trait(sema);
            if (elems().empty()) {
                return trait;
            } else {
                std::vector<Type> type_args;
                for (auto e : elems())
                    type_args.push_back(e->to_type(sema));

                return trait->instantiate(type_args);
            }
        } else
            sema.error(this) << "cannot convert a type variable into a trait instance\n";
    }
    return sema.trait_error();
#endif
}

Trait TraitDecl::calc_trait(TypeSema& sema) const {
    check(sema);
    return trait();
}

Type ValueDecl::calc_type(TypeSema& sema) const {
    check(sema);
    return type();
}

//------------------------------------------------------------------------------

void ModContents::check(TypeSema& sema) const {
    for (auto item : items()) item->check(sema);
}

/*
 * Item::check
 */

void ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
}

void ForeignMod::check(TypeSema& sema) const {
}

void Typedef::check(TypeSema& sema) const {
}

void EnumDecl::check(TypeSema& sema) const {
}

void StaticItem::check(TypeSema& sema) const {
}

void FnDecl::check(TypeSema& sema) const {
#if 0
    // this FnDecl has already been checked
    if (!type().empty())
        return;

    sema.push_scope();
    check_type_params(sema);
    // check parameters
    std::vector<Type> par_types;
    for (const Param* p : fn().params()) {
        sema.insert(p);
        Type pt = p->asttype()->to_type(sema);
        p->set_type(pt);
        par_types.push_back(pt);
    }
    // create FnType
    Type fn_type = sema.fntype(par_types);
    for (auto tp : type_params()) {
        fn_type->add_bound_var(tp->type_var());
    }
    sema.unify(fn_type);
    set_type(fn_type);

    fn().body()->check(sema);
    if (fn().body()->type() != sema.type_noreturn()) {
        Type ret_func = fn_type->elem(fn_type->size() - 1);
        sema.expect_type(fn().body(), create_return_type(sema, this, ret_func), "return");
    }

    sema.pop_scope();
#endif
}

void StructDecl::check(TypeSema& sema) const {
}

void FieldDecl::check(TypeSema&) const {
}

void TraitDecl::check(TypeSema& sema) const {
    // did we already check this trait?
    if ((!trait().empty()))
        return;

    // FEATURE consider super traits and check methods
    trait_ = sema.trait(this);

    check_type_params(sema);
    for (auto tp : type_params()) {
        trait_->add_bound_var(tp->type_var());
    }
}

void Impl::check(TypeSema& sema) const {
#if 0
    if (checked_)
        return;
    else
        checked_ = true;

    sema.push_scope();
    check_type_params(sema);

    Type ftype = for_type()->to_type(sema);

    if (trait() != nullptr) {
        if (auto t = trait()->isa<ASTTypeApp>()) {
            // create impl
            Trait tinst = t->to_trait(sema);
            TraitImpl impl = sema.implement_trait(this, tinst);
            for (auto tp : type_params()) {
                impl->add_bound_var(tp->type_var());
            }

            // add impl to type
            if ((ftype != sema.type_error()) && (tinst != sema.trait_error()))
                ftype->add_implementation(impl);
        } else
            sema.error(trait()) << "expected trait instance.\n";
    }

    // FEATURE check that all methods are implemented
    for (auto fn : methods())
        fn->check(sema);

    sema.pop_scope();
#endif
}

//------------------------------------------------------------------------------

/*
 * Expr::check
 */

void EmptyExpr::check(TypeSema& sema) const { set_type(sema.unit()); }

void BlockExpr::check(TypeSema& sema) const {
    for (auto stmt : stmts())
        stmt->check(sema);

    expr()->check(sema);
    set_type(expr()->type());
}

void LiteralExpr::check(TypeSema& sema) const {
    set_type(sema.primtype(literal2type()));
}

void FnExpr::check(TypeSema& sema) const {
}

void PathExpr::check(TypeSema& sema) const {
#if 0
    // FEATURE consider longer paths
    auto last_item = path()->path_items().back();

    if ((decl_ = sema.lookup(this, last_item->symbol()))) {
        if (auto vdec = decl_->isa<ValueDecl>()) {
            // consider type expressions
            if (!last_item->types().empty()) {
                std::vector<Type> type_args;
                for (const ASTType* t : last_item->types())
                    type_args.push_back(t->to_type(sema));

                set_type(vdec->calc_type(sema)->instantiate(type_args));
            } else
                set_type(vdec->calc_type(sema));
        }
    } else
        set_type(sema.type_error());
#endif
}

void PrefixExpr::check(TypeSema& sema) const {
}

void InfixExpr::check(TypeSema& sema) const {
    lhs()->check(sema);
    rhs()->check(sema);

    Type lhstype = lhs()->type();
    Type rhstype = rhs()->type();

    // FEATURE other cases
    switch (kind()) {
        case EQ:
        case NE:
            sema.match_types(this, lhstype, rhstype);
            set_type(sema.type_bool());
            break;
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
            sema.expect_num(lhs());
            set_type(sema.match_types(this, lhstype, rhstype));
            break;
        default: THORIN_UNREACHABLE;
    }
}

void PostfixExpr::check(TypeSema& sema) const {
}

void FieldExpr::check(TypeSema& sema) const {
}

void CastExpr::check(TypeSema& sema) const {
}

void DefiniteArrayExpr::check(TypeSema& sema) const {
}

void RepeatedDefiniteArrayExpr::check(TypeSema& sema) const {
}

void IndefiniteArrayExpr::check(TypeSema& sema) const {
}

void TupleExpr::check(TypeSema& sema) const {
    std::vector<Type> elems;
    for (auto e : this->elems()) {
        e->check(sema);
        elems.push_back(e->type());
    }
    set_type(sema.tupletype(elems));
}

void StructExpr::check(TypeSema& sema) const {
}

void MapExpr::check(TypeSema& sema) const {
#if 0
    // FEATURE this currently only considers function calls
    lhs()->check(sema);
    Type lhs_type = lhs()->type();

    if (auto fn = lhs_type.isa<FnType>()) {
        bool no_cont = fn->size() == (args().size()+1); // true if this is a normal function call (no continuation)
        if (no_cont || (fn->size() == args().size())) {
            for (size_t i = 0; i < args().size(); ++i) {
                auto arg = args()[i];
                arg->check(sema);
                sema.expect_type(arg, fn->elem(i), "argument");
            }

            // set return type
            if (no_cont) {
                Type ret_func = fn->elem(fn->size() - 1);
                set_type(create_return_type(sema, this, ret_func));
            } else {
                // same number of args as params -> continuation call
                set_type(sema.type_noreturn());
            }
            return;
        } else {
            sema.error(this) << "wrong number of arguments\n";
        }
    } else if (!lhs_type.isa<TypeError>()) {
        // REMINDER new error message if not only fn-types are allowed
        sema.error(lhs()) << "expected function type but found " << lhs_type << "\n";
    }
    assert(type().empty() && "this should only be reached if an error occurred");
    set_type(sema.type_error());
#endif
}

void IfExpr::check(TypeSema& sema) const {
    // assert condition is of type bool
    cond()->check(sema);
    sema.expect_type(cond(), sema.type_bool(), "");

    then_expr()->check(sema);
    else_expr()->check(sema);

    // assert that both branches have the same type and set the type
    set_type(sema.match_types(this, then_expr()->type(), else_expr()->type()));
}

void ForExpr::check(TypeSema& sema) const {
}

//------------------------------------------------------------------------------

/*
 * Stmt::check
 */

void ExprStmt::check(TypeSema& sema) const {
    expr()->check(sema);
}

void ItemStmt::check(TypeSema& sema) const {
    item()->check(sema);
}

void LetStmt::check(TypeSema& sema) const {
    if (init())
        init()->check(sema);
}

//------------------------------------------------------------------------------

bool type_analysis(const ModContents* mod, bool nossa) {
    TypeSema sema(nossa);
    mod->check(sema);
#ifndef NDEBUG
    sema.verify();
#endif
    return sema.result();
}

}
