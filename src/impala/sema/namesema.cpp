#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

class NameSema {
public:
    /**
     * Looks up the current definition of \p symbol.
     * Reports an error at location of \p n if was \p symbol was not found.
     * @return Returns nullptr on failure.
     */
    const Decl* lookup(const ASTNode* n, Symbol);

    /**
     * Maps \p decl's symbol to \p decl.
     * If \p decl's symbol already has a definition in the current scope, an error will be emitted.
     */
    void insert(const Decl* decl);

    /**
     * Checks whether there already exists a \p Symbol \p symbol in the \em current scope.
     * @param symbol The \p Symbol to check.
     * @return The current mapping if the lookup succeeds, nullptr otherwise.
     */
    const Decl* clash(Symbol symbol) const;
    void push_scope() { levels_.push_back(decl_stack_.size()); } ///< Opens a new scope.
    void pop_scope();                                            ///< Discards current scope.

    void bind_head(const Item* item) {
        if (item->is_no_decl()) {
            if (const auto& extern_block = item->isa<ExternBlock>()) {
                for (const auto& fn_decl : extern_block->fn_decls())
                    insert(fn_decl.get());
            }
        } else
            insert(item);
    }

private:
    size_t depth() const { return levels_.size(); }

    thorin::HashMap<Symbol, const Decl*, Symbol::Hash> symbol2decl_;
    std::vector<const Decl*> decl_stack_;
    std::vector<size_t> levels_;

public: // HACK
    int lambda_depth_ = 0;
};

//------------------------------------------------------------------------------

const Decl* NameSema::lookup(const ASTNode* n, Symbol symbol) {
    assert(!symbol.empty() && "symbol is empty");

    if (!symbol.is_anonymous()) {
        auto decl = thorin::find(symbol2decl_, symbol);
        if (decl == nullptr)
            error(n, "'{}' not found in current scope", symbol);
        return decl;
    } else {
        error(n, "identifier '_' is reverserved for anonymous declarations");
        return nullptr;
    }
}

void NameSema::insert(const Decl* decl) {
    assert(!decl->symbol().empty() && "symbol is empty");
    auto symbol = decl->symbol();

    if (!symbol.is_anonymous()) {
        if (auto other = clash(symbol)) {
            error(decl, "symbol '{}' already defined", symbol);
            error(other, "previous location here");
            return;
        }

        assert(clash(symbol) == nullptr && "must not be found");

        auto i = symbol2decl_.find(symbol);
        decl->shadows_ = i != symbol2decl_.end() ? i->second : nullptr;
        decl->depth_ = depth();
        decl_stack_.push_back(decl);
        symbol2decl_[symbol] = decl;
    }
}

const Decl* NameSema::clash(Symbol symbol) const {
    assert(!symbol.empty() && "symbol is empty");
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
 * misc
 */

void ASTTypeParam::bind(NameSema& sema) const {
    for (const auto& bound : bounds())
        bound->bind(sema);
}


void LocalDecl::bind(NameSema& sema) const {
    if (ast_type())
        ast_type()->bind(sema);
    sema.insert(this);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void ASTTypeParamList::bind_ast_type_params(NameSema& sema) const {
    // we need two runs for types like fn[A:T[B], B:T[A]](A, B)
    // first, insert names and generate De Bruijn index
    for (const auto& ast_type_param : ast_type_params()) {
        sema.insert(ast_type_param.get());
        ast_type_param->lambda_depth_ = ++sema.lambda_depth_;
    }

    // then, bind bounds
    for (const auto& ast_type_param : ast_type_params())
        ast_type_param->bind(sema);
}

void ErrorASTType::bind(NameSema&) const {}
void PrimASTType::bind(NameSema&) const {}
void PtrASTType::bind(NameSema& sema) const { referenced_ast_type()->bind(sema); }
void IndefiniteArrayASTType::bind(NameSema& sema) const { elem_ast_type()->bind(sema); }
void DefiniteArrayASTType::bind(NameSema& sema) const { elem_ast_type()->bind(sema); }
void SimdASTType::bind(NameSema& sema) const { elem_ast_type()->bind(sema); }
void Typeof::bind(NameSema& sema) const { expr()->bind(sema); }

void TupleASTType::bind(NameSema& sema) const {
    for (const auto& ast_type_arg : ast_type_args())
        ast_type_arg->bind(sema);
}

void ASTTypeApp::bind(NameSema& sema) const {
    path()->bind(sema);
    for (const auto& ast_type_arg : ast_type_args())
        ast_type_arg->bind(sema);
}

void FnASTType::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    for (const auto& ast_type_arg : ast_type_args())
        ast_type_arg->bind(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModuleDecl::bind(NameSema& ) const {}

void Module::bind(NameSema& sema) const {
    sema.push_scope();
    for (const auto& item : items()) {
        sema.bind_head(item.get());
        if (item->is_named_decl())
            symbol2item_[item->symbol()] = item.get();
    }
    for (const auto& item : items())
        item->bind(sema);
    sema.pop_scope();
}

void ExternBlock::bind(NameSema& sema) const {
    for (const auto& fn_decl : fn_decls())
        fn_decl->bind(sema);
}

void Typedef::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    ast_type()->bind(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void EnumDecl::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    for (const auto& option : option_decls()) {
        option->bind(sema);
        option->enum_decl_ = this;
        option_table_[option->symbol()] = option.get();
    }
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void OptionDecl::bind(NameSema& sema) const {
    sema.insert(this);
    for (const auto& arg : args()) arg->bind(sema);
}

void StaticItem::bind(NameSema& sema) const {
    if (ast_type())
        ast_type()->bind(sema);
    if (init())
        init()->bind(sema);
}

void Fn::fn_bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    for (const auto& param : params()) {
        sema.insert(param.get());
        if (param->ast_type())
            param->ast_type()->bind(sema);
    }
    if (body() != nullptr)
        body()->bind(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void FnDecl::bind(NameSema& sema) const {
    fn_bind(sema);
}

void StructDecl::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    for (const auto& field_decl : field_decls()) {
        field_decl->bind(sema);
        field_table_[field_decl->symbol()] = field_decl.get();
    }
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void FieldDecl::bind(NameSema& sema) const {
    ast_type()->bind(sema);
    sema.insert(this);
}

void TraitDecl::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    for (const auto& t : super_traits())
        t->bind(sema);
    for (const auto& method : methods()) {
        method->bind(sema);
        method_table_[method->symbol()] = method.get();
    }
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void ImplItem::bind(NameSema& sema) const {
    sema.push_scope();
    bind_ast_type_params(sema);
    if (trait())
        trait()->bind(sema);
    ast_type()->bind(sema);
    for (const auto& fn : methods())
        fn->bind(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::bind(NameSema&) const {}

void BlockExprBase::bind(NameSema& sema) const {
    sema.push_scope();
    for (const auto& stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            sema.bind_head(item_stmt->item());
    }
    for (const auto& stmt : stmts())
        stmt->bind(sema);
    expr()->bind(sema);
    sema.pop_scope();
}

void LiteralExpr::bind(NameSema&) const {}
void CharExpr::bind(NameSema&) const {}
void StrExpr::bind(NameSema&) const {}
void FnExpr::bind(NameSema& sema) const { fn_bind(sema); }

void Path::bind(NameSema& sema) const {
    elem(0)->decl_ = sema.lookup(elem(0), elem(0)->symbol());
}

void PathExpr::bind(NameSema& sema) const {
    path()->bind(sema);
}

void PrefixExpr ::bind(NameSema& sema) const {                    rhs()->bind(sema); }
void InfixExpr  ::bind(NameSema& sema) const { lhs()->bind(sema); rhs()->bind(sema); }
void PostfixExpr::bind(NameSema& sema) const { lhs()->bind(sema); }

void FieldExpr::bind(NameSema& sema) const {
    lhs()->bind(sema);
    // don't bind symbol here as it depends on lhs' type - must be done in TypeSema
}

void ExplicitCastExpr::bind(NameSema& sema) const {
    src()->bind(sema);
    ast_type()->bind(sema);
}

void DefiniteArrayExpr::bind(NameSema& sema) const {
    for (const auto& arg : args())
        arg->bind(sema);
}

void RepeatedDefiniteArrayExpr::bind(NameSema& sema) const {
    value()->bind(sema);
}

void IndefiniteArrayExpr::bind(NameSema& sema) const {
    dim()->bind(sema);
    elem_ast_type()->bind(sema);
}

void TupleExpr::bind(NameSema& sema) const {
    for (const auto& arg : args())
        arg->bind(sema);
}

void SimdExpr::bind(NameSema& sema) const {
    for (const auto& arg : args())
        arg->bind(sema);
}

void StructExpr::bind(NameSema& sema) const {
    ast_type_app()->bind(sema);
    for (const auto& elem : elems())
        elem->expr()->bind(sema);
}

void TypeAppExpr::bind(NameSema& sema) const {
    lhs()->bind(sema);
    for (const auto& ast_type_arg : ast_type_args())
        ast_type_arg->bind(sema);
}

void MapExpr::bind(NameSema& sema) const {
    lhs()->bind(sema);
    for (const auto& arg : args())
        arg->bind(sema);
}

void IfExpr::bind(NameSema& sema) const {
    cond()->bind(sema);
    then_expr()->bind(sema);
    else_expr()->bind(sema);
}

void MatchExpr::bind(NameSema& sema) const {
    expr()->bind(sema);
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        sema.push_scope();
        arm(i)->ptrn()->bind(sema);
        arm(i)->expr()->bind(sema);
        sema.pop_scope();
    }
}

void WhileExpr::bind(NameSema& sema) const {
    cond()->bind(sema);
    sema.push_scope();
    break_decl()->bind(sema);
    continue_decl()->bind(sema);
    body()->bind(sema);
    sema.pop_scope();
}

void ForExpr::bind(NameSema& sema) const {
    expr()->bind(sema);
    sema.push_scope();
    break_decl()->bind(sema);
    fn_expr()->bind(sema);
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * patterns
 */

void TuplePtrn::bind(NameSema& sema) const {
    for (const auto& elem : elems()) {
        elem->bind(sema);
    }
}

void IdPtrn::bind(NameSema& sema) const {
    local()->bind(sema);
}

void EnumPtrn::bind(NameSema& sema) const {
    path()->bind(sema);
    for (const auto& arg : args()) {
        arg->bind(sema);
    }
}

void LiteralPtrn::bind(NameSema&) const {}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::bind(NameSema& sema) const { expr()->bind(sema); }
void ItemStmt::bind(NameSema& sema) const { item()->bind(sema); }
void LetStmt::bind(NameSema& sema) const {
    if (init())
        init()->bind(sema);
    ptrn()->bind(sema);
}
void AsmStmt::bind(NameSema& sema) const {
    for (const auto& output : outputs())
        output->expr()->bind(sema);
    for (const auto& input : inputs())
        input->expr()->bind(sema);
}

//------------------------------------------------------------------------------

void name_analysis(const Module* module) {
    NameSema sema;
    module->bind(sema);
}

//------------------------------------------------------------------------------

}
