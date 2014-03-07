#include "impala/ast.h"
#include "impala/sema/errorhandler.h"

namespace impala {

//------------------------------------------------------------------------------

class NameSema : public ErrorHandler {
public:
    /** 
     * @brief Looks up the current definition of \p symbol.
     * @return Returns nullptr on failure.
     */
    const Decl* lookup(Symbol symbol) const;

    /// Lookups \p symbol and reports an error at location of \p n if was not found
    const Decl* lookup(const ASTNode* n, Symbol);

    /** 
     * @brief Maps \p decl's symbol to \p decl.
     * 
     * If \p decl's symbol already has a definition in the current scope, an error will be emitted.
     */
    void insert(const Decl* decl);

    /** 
     * @brief Checks whether there already exists a \p Symbol \p symbol in the \em current scope.
     * @param symbol The \p Symbol to check.
     * @return The current mapping if the lookup succeeds, nullptr otherwise.
     */
    const Decl* clash(Symbol symbol) const;
    void push_scope() { levels_.push_back(decl_stack_.size()); } ///< Opens a new scope.
    void pop_scope();                                            ///< Discards current scope.

private:
    size_t depth() const { return levels_.size(); }

    thorin::HashMap<Symbol, const Decl*> symbol2decl_;
    std::vector<const Decl*> decl_stack_;
    std::vector<size_t> levels_;
};

//------------------------------------------------------------------------------

const Decl* NameSema::lookup(Symbol symbol) const {
    auto i = symbol2decl_.find(symbol);
    return i != symbol2decl_.end() ? i->second : nullptr;
}

const Decl* NameSema::lookup(const ASTNode* n, Symbol symbol) {
    auto decl = lookup(symbol);
    if (decl == nullptr)
        error(n) << '\'' << symbol << "' not found in current scope\n";
    return decl;
}

void NameSema::insert(const Decl* decl) {
    if (const Decl* other = clash(decl->symbol())) {
        error(decl) << "symbol '" << decl->symbol() << "' already defined\n";
        error(other) << "previous location here\n";
        return;
    }

    Symbol symbol = decl->symbol();
    assert(clash(symbol) == nullptr && "must not be found");

    auto i = symbol2decl_.find(symbol);
    decl->shadows_ = i != symbol2decl_.end() ? i->second : nullptr;
    decl->depth_ = depth();
    decl_stack_.push_back(decl);
    symbol2decl_[symbol] = decl;
}

const Decl* NameSema::clash(Symbol symbol) const {
    if (auto decl = thorin::find(symbol2decl_, symbol))
        return (decl && decl->depth() == depth()) ? decl : nullptr;
    return nullptr;
}

void NameSema::pop_scope() {
    size_t level = levels_.back();
    for (size_t i = level, e = decl_stack_.size(); i != e; ++i) {
        const Decl* decl = decl_stack_[i];
        symbol2decl_[decl->symbol()] = decl->shadows();
    }

    decl_stack_.resize(level);
    levels_.pop_back();
}

//------------------------------------------------------------------------------

/*
 * ASTType::to_type
 */

void ParametricASTType::check_type_params(NameSema& sema) const {
    for (const TypeParam* tp : type_params())
        sema.insert(tp);

    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>())
                trait_inst->to_trait(sema);
            else
                sema.error(tp) << "Bounds must be trait instances, not types\n";
        }
    }
}

void ErrorASTType::to_type(NameSema& sema) const {}
void PrimASTType::to_type(NameSema& sema) const {}
void PtrASTType::to_type(NameSema& sema) const { referenced_type()->to_type(sema); }
void IndefiniteArrayASTType::to_type(NameSema& sema) const { elem_type()->to_type(sema); }
void DefiniteArrayASTType::to_type(NameSema& sema) const { elem_type()->to_type(sema); }

void TupleASTType::to_type(NameSema& sema) const {
    for (auto elem : this->elems())
        elem->to_type(sema);
}

void ASTTypeApp::to_type(NameSema& sema) const {
    if (auto decl = sema.lookup(this, symbol())) {
        if (auto tp = decl->isa<TypeParam>()) {
            // TODO
            //assert(elems().empty());
            //assert(!tp->type_var().empty());
            //return tp->type_var();
        } else
            sema.error(this) << "cannot convert a trait instance into a type\n";
    }
}

void FnASTType::to_type(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    sema.pop_scope();
}

Trait ASTTypeApp::to_trait(NameSema& sema) const {
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
 * Item::check_head
 */

void ModDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void ModContents::check(NameSema& sema) const {
    for (auto item : items()) item->check_head(sema);
    for (auto item : items()) item->check(sema);
}

void ForeignMod::check_head(NameSema& sema) const {
    sema.insert(this);
}

void EnumDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void FnDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void StaticItem::check_head(NameSema& sema) const {
    sema.insert(this);
}

void StructDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void TraitDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void Typedef::check_head(NameSema& sema) const {
    sema.insert(this);
}

void Impl::check_head(NameSema& sema) const {
    sema.push_impl(this);
}

//------------------------------------------------------------------------------

/*
 * Item::check
 */

void ModDecl::check(NameSema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
}

void ForeignMod::check(NameSema& sema) const {
}

void Typedef::check(NameSema& sema) const {
}

void EnumDecl::check(NameSema& sema) const {
}

void StaticItem::check(NameSema& sema) const {
}

void FnDecl::check(NameSema& sema) const {
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

void StructDecl::check(NameSema& sema) const {
}

void TraitDecl::check(NameSema& sema) const {
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

void Impl::check(NameSema& sema) const {
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

//------------------------------------------------------------------------------

/*
 * Expr::check
 */

void EmptyExpr::check(NameSema& sema) const {
    // empty expression returns unit - the empty tuple type '()'
    set_type(sema.unit());
}

void BlockExpr::check(NameSema& sema) const {
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            item_stmt->item()->check_head(sema);
    }

    for (auto stmt : stmts())
        stmt->check(sema);

    expr()->check(sema);
    set_type(expr()->type());
}

void LiteralExpr::check(NameSema& sema) const {
    set_type(sema.primtype(literal2type()));
}

void FnExpr::check(NameSema& sema) const {
}

void PathExpr::check(NameSema& sema) const {
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

void PrefixExpr::check(NameSema& sema) const {
}

void InfixExpr::check(NameSema& sema) const {
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

void PostfixExpr::check(NameSema& sema) const {
}

void FieldExpr::check(NameSema& sema) const {
}

void CastExpr::check(NameSema& sema) const {
}

void DefiniteArrayExpr::check(NameSema& sema) const {
}

void RepeatedDefiniteArrayExpr::check(NameSema& sema) const {
}

void IndefiniteArrayExpr::check(NameSema& sema) const {
}

void TupleExpr::check(NameSema& sema) const {
    std::vector<Type> elems;
    for (auto e : this->elems()) {
        e->check(sema);
        assert(!e->type().empty());
        elems.push_back(e->type());
    }
    set_type(sema.tupletype(elems));
}

void StructExpr::check(NameSema& sema) const {
}

void MapExpr::check(NameSema& sema) const {
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

void IfExpr::check(NameSema& sema) const {
    // assert condition is of type bool
    cond()->check(sema);
    expect_type(sema, cond(), sema.type_bool(), "");

    then_expr()->check(sema);
    else_expr()->check(sema);

    // assert that both branches have the same type and set the type
    set_type(match_types(sema, this, then_expr()->type(), else_expr()->type()));
    assert(!type().empty());
}

void ForExpr::check(NameSema& sema) const {
}

//------------------------------------------------------------------------------

/*
 * Stmt::check
 */

void ExprStmt::check(NameSema& sema) const {
    expr()->check(sema);
}

void ItemStmt::check(NameSema& sema) const {
    item()->check(sema);
}

void LetStmt::check(NameSema& sema) const {
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
