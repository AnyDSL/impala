#include <iostream>

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/scopetable.h"
#include "impala/sema/typetable.h"

namespace impala {

class Sema : public ScopeTable, public TypeTable {
public:
    Sema(bool nossa)
        : cur_fn_(nullptr)
        , nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    const Fn* cur_fn() const { return cur_fn_; }

private:
    const Fn* cur_fn_;
    bool nossa_;
};

void expect_num(Sema& sema, const Expr* exp) {
    Type t = exp->type();
    assert(!t.empty());

    if (t == sema.type_error())
        return;

    // TODO realize this with subtyping
    if ((t != sema.type_int()) && (t != sema.type_int8()) && (t != sema.type_int16()) && (t != sema.type_int32()) &&
            (t != sema.type_int64()) && (t != sema.type_float()) && (t != sema.type_double()))
        sema.error(exp) << "Expected number type but found " << t << "\n";
}

Type match_types(Sema& sema, const Expr* pos, Type t1, Type t2) {
    assert(!t1.empty());
    assert(!t2.empty());

    auto error = sema.type_error();
    if (t1 == error || t2 == error)
        return error;

    // TODO consider subtyping of primitive types and return the "smallest common type"
    if (t1 == t2) {
        return t1;
    } else {
        sema.error(pos) << "Types do not match: " << t1 << " != " << t2 << "\n";
        return error;
    }
}

inline void expect_type(Sema& sema, const Expr* found, Type expected) {
    assert(!expected.empty());
    assert(!found->type().empty());
    assert(expected != sema.type_error());

    if (found->type() == sema.type_error())
        return;
    if (found->type() != expected)
        sema.error(found) << "Wrong type; expected " << expected << " but found " << found->type() << "\n";
}

//------------------------------------------------------------------------------

void ParametricASTType::check_type_params(Sema& sema) const {
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

Type ErrorASTType::to_type(Sema& sema) const { return sema.type_error(); }

Type PrimASTType::to_type(Sema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.primtype(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::to_type(Sema& sema) const {
    return Type(); // FEATURE
}

Type IndefiniteArrayASTType::to_type(Sema& sema) const {
    return Type(); // FEATURE
}

Type DefiniteArrayASTType::to_type(Sema& sema) const {
    return Type(); // FEATURE
}

Type TupleASTType::to_type(Sema& sema) const {
    std::vector<Type> elems;
    for (auto e : this->elems())
        elems.push_back(e->to_type(sema));

    return sema.tupletype(elems);
}

Type ASTTypeApp::to_type(Sema& sema) const {
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

Type FnASTType::to_type(Sema& sema) const {
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

Trait ASTTypeApp::to_trait(Sema& sema) const {
    if (auto decl = sema.lookup(this, symbol())) {
        if (auto trait_decl = decl->isa<TraitDecl>()) {
            std::vector<Type> type_args;
            for (auto e : elems())
                type_args.push_back(e->to_type(sema));

            //return sema.instantiate_trait(trait_decl->calc_trait(sema), type_args); FIXME
            return trait_decl->calc_trait(sema);
        } else
            sema.error(this) << "cannot convert a type variable into a trait instance\n";
    }
    // TODO return error_trait_instance <- we need sth like that
    return Trait();
}

//------------------------------------------------------------------------------

/*
 * items - check_head
 */

void ModDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void ModContents::check(Sema& sema) const {
    for (auto item : items()) item->check_head(sema);
    for (auto item : items()) item->check(sema);
}

void ForeignMod::check_head(Sema& sema) const {
    sema.insert(this);
}

void EnumDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void FnDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void StaticItem::check_head(Sema& sema) const {
    sema.insert(this);
}

void StructDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void TraitDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void Typedef::check_head(Sema& sema) const {
    sema.insert(this);
}

void Impl::check_head(Sema& sema) const {}

/*
 * items - check
 */

void ModDecl::check(Sema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
}

void ForeignMod::check(Sema& sema) const {
}

void Typedef::check(Sema& sema) const {
}

void EnumDecl::check(Sema& sema) const {
}

void StaticItem::check(Sema& sema) const {
}

void FnDecl::check(Sema& sema) const {
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

    // CHECK set sema.cur_fn_?
    fn().body()->check(sema);

    // FEATURE check for correct return type
    sema.pop_scope();
}

void StructDecl::check(Sema& sema) const {
}

void TraitDecl::check(Sema& sema) const {
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

void Impl::check(Sema& sema) const {
    sema.push_scope();
    check_type_params(sema);

    // TODO currently symbol() gives the trait name, this does not handle stuff like 'impl T[int] for S {}'
    if (auto decl = sema.lookup(this, Symbol() /*TODO*/)) {
        if (auto trait = decl->isa<TraitDecl>()) {
            // create TraitImpl
            /*Trait tinst = sema.instantiate_trait(trait->calc_trait(sema), {}); FIXME
            const TraitImpl* impl = sema.implement_trait(this, tinst);

            // add impl to type
            Type t = for_type()->to_type(sema);
            if (t != sema.type_error())
                t->add_implementation(impl);

            // FEATURE check that all methods are implemented
            for (auto fn : methods())
                fn->check(sema);*/
        } else
            sema.error(decl) << decl << " is not the name of a trait.\n";
    }

    sema.pop_scope();
}

/*
 * expressions
 */

void EmptyExpr::check(Sema& sema) const {
    // empty expression returns unit - the empty tuple type '()'
    set_type(sema.unit());
}

void BlockExpr::check(Sema& sema) const {
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            item_stmt->item()->check_head(sema);
    }

    for (auto stmt : stmts())
        stmt->check(sema);

    expr()->check(sema);
    assert(!expr()->type().empty());
    set_type(expr()->type());
}

void LiteralExpr::check(Sema& sema) const {
    set_type(sema.primtype(literal2type()));
}

void FnExpr::check(Sema& sema) const {
}

void PathExpr::check(Sema& sema) const {
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

void PrefixExpr::check(Sema& sema) const {
}

void InfixExpr::check(Sema& sema) const {
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

void PostfixExpr::check(Sema& sema) const {
}

void FieldExpr::check(Sema& sema) const {
}

void CastExpr::check(Sema& sema) const {
}

void DefiniteArrayExpr::check(Sema& sema) const {
}

void RepeatedDefiniteArrayExpr::check(Sema& sema) const {
}

void IndefiniteArrayExpr::check(Sema& sema) const {
}

void TupleExpr::check(Sema& sema) const {
    std::vector<Type> elems;
    for (auto e : this->elems()) {
        e->check(sema);
        assert(!e->type().empty());
        elems.push_back(e->type());
    }
    set_type(sema.tupletype(elems));
}

void StructExpr::check(Sema& sema) const {
}

void MapExpr::check(Sema& sema) const {
    // FEATURE this currently only considers function calls
    lhs()->check(sema);
    Type lhs_type = lhs()->type();
    assert(!lhs_type.empty());

    switch (lhs_type->kind()) {
        case Type_error:
            set_type(sema.type_error());
            return;
        case Type_fn:
            if ((lhs_type->size() != (args().size()+1)) && (lhs_type->size() != args().size())) {
                // CHECK how would the result type look like if the continuation is explicitly passed?
                sema.error(this) << "Wrong number of arguments\n";
                set_type(sema.type_error());
            } else {
                for (size_t i = 0; i < args().size(); ++i) {
                    auto arg = args()[i];
                    arg->check(sema);
                    expect_type(sema, arg, lhs_type->elem(i));
                }

                // set return type
                Type ret_func = lhs_type->elem(lhs_type->size() - 1);
                assert(ret_func->kind() == Type_fn); // CHECK can there be function types w/o return function?

                switch (ret_func->size()) {
                case 0:
                    set_type(sema.unit());
                    break;
                case 1:
                    set_type(ret_func->elem(0));
                    break;
                default:
                    std::vector<Type> ret_types;
                    for (auto t : ret_func->elems())
                        ret_types.push_back(t);
                    set_type(sema.tupletype(ret_types));
                    break;
                }
            }
            break;
        default:
            set_type(sema.type_error());
    }
    assert(!type().empty());
}

void IfExpr::check(Sema& sema) const {
    // assert condition is of type bool
    cond()->check(sema);
    expect_type(sema, cond(), sema.type_bool());

    then_expr()->check(sema);

    if (has_else()) {
        else_expr()->check(sema);

        // assert that both branches have the same type and set the type
        set_type(match_types(sema, this, then_expr()->type(), else_expr()->type()));
    } else {
        // CHECK what is the type of this IfExpr now?
    }

    assert(!type().empty());
}

void ForExpr::check(Sema& sema) const {
}

/*
 * statements
 */

void ExprStmt::check(Sema& sema) const {
    expr()->check(sema);
}

void ItemStmt::check(Sema& sema) const {
    item()->check(sema);
}

void LetStmt::check(Sema& sema) const {
    if (init())
        init()->check(sema);
}

//------------------------------------------------------------------------------

bool check(const ModContents* mod, bool nossa) {
    Sema sema(nossa);
    mod->check(sema);
    return sema.result();
}

}
