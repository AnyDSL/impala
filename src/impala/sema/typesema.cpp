#include <iostream>

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/typetable.h"

namespace impala {

class TypeSema : public TypeTable {
public:
    TypeSema(const bool result, const bool nossa)
        : TypeTable(result)
        , nossa_(nossa)
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

private:
    bool nossa_;
    std::vector<const Impl*> impls_;
};

void expect_num(TypeSema& sema, const Expr* exp) {
    Type t = exp->type();
    assert(!t.empty());

    if (t == sema.type_error())
        return;

    if ((t != sema.type_int()) && (t != sema.type_int8()) && (t != sema.type_int16()) && (t != sema.type_int32()) &&
            (t != sema.type_int64()) && (t != sema.type_float()) && (t != sema.type_double()))
        sema.error(exp) << "Expected number type but found " << t << "\n";
}

Type match_types(TypeSema& sema, const Expr* pos, Type t1, Type t2) {
    assert(!t1.empty());
    assert(!t2.empty());

    Type error = sema.type_error();
    if (t1 == error || t2 == error)
        return error;

    if (t1 == t2) {
        return t1;
    } else {
        sema.error(pos) << "Types do not match: " << t1 << " != " << t2 << "\n";
        return error;
    }
}

inline void expect_type(TypeSema& sema, const Expr* found, Type expected, std::string typetype) {
    assert(!expected.empty());
    assert(!found->type().empty());

    if (found->type() == sema.type_error() || expected == sema.type_error())
        return;
    if (found->type() != expected)
        sema.error(found) << "Wrong " << typetype << " type; expected " << expected << " but found " << found->type() << "\n";
}

inline Type create_return_type(TypeSema& sema, const ASTNode* node, Type ret_func) {
    if (ret_func.isa<FnType>()) {
        if (ret_func->size() == 1) {
            return ret_func->elem(0);
        } else {
            std::vector<Type> ret_types;
            for (auto t : ret_func->elems())
                ret_types.push_back(t);
            return sema.tupletype(ret_types);
        }
    } else {
        sema.error(node) << "last argument is not a continuation function\n";
        return sema.type_error();
    }
}

//------------------------------------------------------------------------------

void ParametricASTType::check_type_params(TypeSema& sema) const {
    // we need two runs for types like fn[A:T[B], B:T[A]](A, B)

    for (const TypeParam* tp : type_params()) {
        tp->type_var_ = sema.typevar();
        sema.insert(tp);
    }

    // check bounds
    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>()) {
                tp->type_var()->add_bound(trait_inst->to_trait(sema));
            } else {
                sema.error(tp) << "Bounds must be trait instances, not types\n";
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
    if (auto decl = sema.lookup(this, symbol())) {
        if (auto tp = decl->isa<TypeParam>()) {
            assert(elems().empty());
            assert(!tp->type_var().empty());
            return tp->type_var();
        } else
            sema.error(this) << "cannot convert a trait instance into a type\n";
    }

    return sema.type_error();
}

Type FnASTType::to_type(TypeSema& sema) const {
    sema.push_scope();

    check_type_params(sema);

    std::vector<Type> params;
    for (auto p : elems())
        params.push_back(p->to_type(sema));

    FnType t = sema.fntype(params);
    for (auto tp : type_params())
        t->add_bound_var(tp->type_var());

    sema.pop_scope();

    return t;
}

Trait ASTTypeApp::to_trait(TypeSema& sema) const {
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
}

//------------------------------------------------------------------------------

/*
 * items - check_head
 */

void ModDecl::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void ModContents::check(TypeSema& sema) const {
    for (auto item : items()) item->check_head(sema);
    for (auto item : items()) item->check(sema);
}

void ForeignMod::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void EnumDecl::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void FnDecl::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void StaticItem::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void StructDecl::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void TraitDecl::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void Typedef::check_head(TypeSema& sema) const {
    sema.insert(this);
}

void Impl::check_head(TypeSema& sema) const {
    sema.push_impl(this);
}

/*
 * items - check
 */

void ModDecl::check(TypeSema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
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
        assert(!tp->type_var().empty());
        fn_type->add_bound_var(tp->type_var());
    }
    sema.unify(fn_type);
    set_type(fn_type);

    fn().body()->check(sema);
    if (fn().body()->type() != sema.type_noreturn()) {
        Type ret_func = fn_type->elem(fn_type->size() - 1);
        expect_type(sema, fn().body(), create_return_type(sema, this, ret_func), "return");
    }

    sema.pop_scope();
}

void StructDecl::check(TypeSema& sema) const {
}

void TraitDecl::check(TypeSema& sema) const {
    // did we already check this trait?
    if ((!trait().empty()))
        return;

    // FEATURE consider super traits and check methods
    trait_ = sema.trait(this);

    check_type_params(sema);
    for (auto tp : type_params()) {
        assert(!tp->type_var().empty());
        trait_->add_bound_var(tp->type_var());
    }
}

void Impl::check(TypeSema& sema) const {
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
                assert(!tp->type_var().empty());
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
}

/*
 * expressions
 */

void EmptyExpr::check(TypeSema& sema) const {
    // empty expression returns unit - the empty tuple type '()'
    set_type(sema.unit());
}

void BlockExpr::check(TypeSema& sema) const {
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            item_stmt->item()->check_head(sema);
    }

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
            match_types(sema, this, lhstype, rhstype);
            set_type(sema.type_bool());
            break;
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
            expect_num(sema, lhs());
            set_type(match_types(sema, this, lhstype, rhstype));
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
        assert(!e->type().empty());
        elems.push_back(e->type());
    }
    set_type(sema.tupletype(elems));
}

void StructExpr::check(TypeSema& sema) const {
}

void MapExpr::check(TypeSema& sema) const {
    // FEATURE this currently only considers function calls
    lhs()->check(sema);
    Type lhs_type = lhs()->type();
    assert(!lhs_type.empty());

    if (auto fn = lhs_type.isa<FnType>()) {
        bool no_cont = fn->size() == (args().size()+1); // true if this is a normal function call (no continuation)
        if (no_cont || (fn->size() == args().size())) {
            for (size_t i = 0; i < args().size(); ++i) {
                auto arg = args()[i];
                arg->check(sema);
                expect_type(sema, arg, fn->elem(i), "argument");
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
            sema.error(this) << "Wrong number of arguments\n";
        }
    } else if (!lhs_type.isa<TypeError>()) {
        // REMINDER new error message if not only fn-types are allowed
        sema.error(lhs()) << "Expected function type but found " << lhs_type << "\n";
    }
    assert(type().empty() && "This should only be reached if an error occurred");
    set_type(sema.type_error());
}

void IfExpr::check(TypeSema& sema) const {
    // assert condition is of type bool
    cond()->check(sema);
    expect_type(sema, cond(), sema.type_bool(), "");

    then_expr()->check(sema);
    else_expr()->check(sema);

    // assert that both branches have the same type and set the type
    set_type(match_types(sema, this, then_expr()->type(), else_expr()->type()));
    assert(!type().empty());
}

void ForExpr::check(TypeSema& sema) const {
}

/*
 * statements
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

bool check(const ModContents* mod, bool nossa) {
    TypeSema sema(nossa);
    mod->check(sema);
    return sema.result();
}

}
