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

    void check_head(const Item* item) {
        if (auto decl = item->isa<Decl>())
            insert(decl);
        else if (auto extern_block = item->isa<ExternBlock>()) {
            for (auto fn : extern_block->fns())
                insert(fn);
        }
    }

private:
    size_t depth() const { return levels_.size(); }

    thorin::HashMap<Symbol, const Decl*> symbol2decl_;
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
            error(n, "'%' not found in current scope", symbol);
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
            error(decl, "symbol '%' already defined", symbol);
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

void ASTTypeParam::check(NameSema& sema) const {
    for (auto bound : bounds())
        bound->check(sema);
}


void LocalDecl::check(NameSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    sema.insert(this);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void ASTTypeParamList::check_ast_type_params(NameSema& sema) const {
    // we need two runs for types like fn[A:T[B], B:T[A]](A, B)
    // first, insert names and generate De Bruijn index
    for (auto ast_type_param : ast_type_params()) {
        sema.insert(ast_type_param);
        ast_type_param->lambda_depth_ = ++sema.lambda_depth_;
    }

    // then, check bounds
    for (auto ast_type_param : ast_type_params())
        ast_type_param->check(sema);
}

void ErrorASTType::check(NameSema&) const {}
void PrimASTType::check(NameSema&) const {}
void PtrASTType::check(NameSema& sema) const { referenced_ast_type()->check(sema); }
void IndefiniteArrayASTType::check(NameSema& sema) const { elem_ast_type()->check(sema); }
void DefiniteArrayASTType::check(NameSema& sema) const { elem_ast_type()->check(sema); }
void SimdASTType::check(NameSema& sema) const { elem_ast_type()->check(sema); }
void Typeof::check(NameSema& sema) const { expr()->check(sema); }

void TupleASTType::check(NameSema& sema) const {
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void ASTTypeApp::check(NameSema& sema) const {
    path()->check(sema);
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void FnASTType::check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModDecl::check(NameSema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
}

void ModContents::check(NameSema& sema) const {
    for (auto item : items()) {
        sema.check_head(item);
        if (auto named_item = item->isa<NamedItem>())
            item_table_[named_item->item_symbol()] = named_item;
    }
    for (auto item : items())
        item->check(sema);
}

void ExternBlock::check(NameSema& sema) const {
    for (auto fn : fns())
        fn->check(sema);
}

void Typedef::check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    ast_type()->check(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void EnumDecl::check(NameSema&) const {}

void StaticItem::check(NameSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    if (init())
        init()->check(sema);
}

void Fn::fn_check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    for (auto param : params()) {
        sema.insert(param);
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void FnDecl::check(NameSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void FieldDecl::check(NameSema& sema) const {
    ast_type()->check(sema);
    sema.insert(this);
}

void TraitDecl::check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    for (auto t : super_traits())
        t->check(sema);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

void ImplItem::check(NameSema& sema) const {
    sema.push_scope();
    check_ast_type_params(sema);
    if (trait())
        trait()->check(sema);
    ast_type()->check(sema);
    for (auto fn : methods())
        fn->check(sema);
    sema.lambda_depth_ -= num_ast_type_params();
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::check(NameSema&) const {}

void BlockExprBase::check(NameSema& sema) const {
    sema.push_scope();
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            sema.check_head(item_stmt->item());
    }
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
    sema.pop_scope();
}

void LiteralExpr::check(NameSema&) const {}
void CharExpr::check(NameSema&) const {}
void StrExpr::check(NameSema&) const {}
void FnExpr::check(NameSema& sema) const { fn_check(sema); }

void Path::Elem::check(NameSema& sema) const {
    decl_ = sema.lookup(this, symbol());
}

void Path::check(NameSema& sema) const {
    for (auto elem : elems())
        elem->check(sema);
}

void PathExpr::check(NameSema& sema) const {
    path()->check(sema);
    if (path()->decl()) {
        value_decl_ = path()->decl()->isa<ValueDecl>();
        if (!value_decl_)
            error(this, "'%' is not a value", path());
    }
}

void PrefixExpr::check(NameSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(NameSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(NameSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    // don't check symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    ast_type()->check(sema);
}

void DefiniteArrayExpr::check(NameSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void RepeatedDefiniteArrayExpr::check(NameSema& sema) const {
    value()->check(sema);
}

void IndefiniteArrayExpr::check(NameSema& sema) const {
    dim()->check(sema);
    elem_ast_type()->check(sema);
}

void TupleExpr::check(NameSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void SimdExpr::check(NameSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void StructExpr::check(NameSema& sema) const {
    ast_type_app()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema);
}

void TypeAppExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void MapExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    for (const auto& arg : args())
        arg->check(sema);
}

void IfExpr::check(NameSema& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void WhileExpr::check(NameSema& sema) const {
    cond()->check(sema);
    sema.push_scope();
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema);
    sema.pop_scope();
}

void ForExpr::check(NameSema& sema) const {
    expr()->check(sema);
    sema.push_scope();
    break_decl()->check(sema);
    fn_expr()->check(sema);
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(NameSema& sema) const { expr()->check(sema); }
void ItemStmt::check(NameSema& sema) const { item()->check(sema); }
void LetStmt::check(NameSema& sema) const {
    if (init())
        init()->check(sema);
    local()->check(sema);
}

//------------------------------------------------------------------------------

void name_analysis(const ModContents* mod) {
    NameSema sema;
    mod->check(sema);
}

//------------------------------------------------------------------------------

}
