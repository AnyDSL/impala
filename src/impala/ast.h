#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/types.h"

#include "impala/impala.h"
#include "impala/loc.h"
#include "impala/token.h"
#include "impala/sema/type.h"

namespace thorin {
    class Lam;
}

namespace impala {

class ASTType;
class ASTTypeApp;
class ASTTypeParam;
class Decl;
class Expr;
class FieldDecl;
class OptionDecl;
class Fn;
class FnDecl;
class LocalDecl;
class Param;
class Ptrn;
class Stmt;
class PrefixExpr;
class RValueExpr;

class NameSema;
class InferSema;
class TypeSema;
class CodeGen;

typedef ArrayRef<std::unique_ptr<const ASTType>> ASTTypeArgs;
typedef std::deque<std::unique_ptr<const Expr>> Exprs;
typedef std::deque<std::unique_ptr<const Ptrn>> Ptrns;
typedef std::vector<Symbol> Symbols;
typedef std::vector<const LocalDecl*> LocalDecls;
typedef std::vector<std::string> Strings;
typedef std::vector<std::unique_ptr<const ASTType>> ASTTypes;
typedef std::vector<std::unique_ptr<const ASTTypeApp>> ASTTypeApps;
typedef std::vector<std::unique_ptr<const ASTTypeParam>> ASTTypeParams;
typedef std::vector<std::unique_ptr<const FieldDecl>> FieldDecls;
typedef std::vector<std::unique_ptr<const OptionDecl>> OptionDecls;
typedef std::vector<std::unique_ptr<const FnDecl>> FnDecls;
typedef std::vector<std::unique_ptr<const Param>> Params;
typedef std::vector<std::unique_ptr<const Stmt>> Stmts;
typedef std::vector<char> Chars;
typedef thorin::HashMap<Symbol, const FieldDecl*> FieldTable;
typedef thorin::HashMap<Symbol, const OptionDecl*> OptionTable;
typedef thorin::HashMap<Symbol, const FnDecl*> MethodTable;
typedef thorin::HashMap<Symbol, const Item*> Symbol2Item;

/**
 * Assigns @p src's @p Expr::back_ref_ to @p dst and returns @p src.
 * In a typical @p ASTNode owning an @p Expr you should have a member:
@code{.cpp}
std::unique_ptr<const Expr> expr_;
@endcode
The constructor should look like this:
@code{.cpp}
MyExpr(Loc loc, ..., const Expr* expr, ...)
    : Expr(loc)
    , ...
    , expr_(dock(expr_, expr))
{}
@endcode
 * @see interlope
 */
const Expr* dock(std::unique_ptr<const Expr>& dst, const Expr* src);

//------------------------------------------------------------------------------

/*
 * helpers and mixins
 */

/// Aggregate with all entities which have a visibility.
class Visibility {
public:
    enum {
        None,
        Pub = Token::PUB,
        Priv = Token::PRIV
    };

    Visibility(int visibility)
        : visibility_(visibility)
    {}

    const char* str();
    bool is_pub() const { return visibility_ == Pub; }
    bool is_priv() const { return visibility_ == Priv; }
    Stream& stream(Stream&) const;

private:
    int visibility_;
};

/// Mixin for all entities which have a list of @p TypeParam%s: [T1, T2 : A + B[...], ...].
class ASTTypeParamList {
public:
    ASTTypeParamList(ASTTypeParams&& ast_type_params)
        : ast_type_params_(std::move(ast_type_params))
    {}

    size_t num_ast_type_params() const { return ast_type_params_.size(); }
    const ASTTypeParam* ast_type_param(size_t i) const { return ast_type_params_[i].get(); }
    const ASTTypeParams& ast_type_params() const { return ast_type_params_; }
    Stream& stream_ast_type_params(Stream&) const;

protected:
    void bind_ast_type_params(NameSema&) const;
    void infer_ast_type_params(InferSema&) const;
    void check_ast_type_params(TypeSema&) const;

    ASTTypeParams ast_type_params_;
};

//------------------------------------------------------------------------------

class ASTNode : public thorin::RTTICast<ASTNode> {
public:
    ASTNode() = delete;
    ASTNode(const ASTNode&) = delete;
    ASTNode(ASTNode&&) = delete;
    ASTNode(Loc loc);
    virtual ~ASTNode() { assert(loc_.is_set()); }

    size_t gid() const { return gid_; }
    Loc loc() const { return loc_; }
    virtual Stream& stream(Stream&) const = 0;
    void dump() const;
    Stream& operator<<(Stream& s) const { return stream(s); }

private:
    static size_t gid_counter_;

    size_t gid_;
    Loc loc_;
};

template<class... Args>
void warning(const ASTNode* n, const char* fmt, Args... args) { warning(n->loc(), fmt, args...); }
template<class... Args>
void error  (const ASTNode* n, const char* fmt, Args... args) { error  (n->loc(), fmt, args...); }

class Identifier : public ASTNode {
public:
    Identifier(Loc loc, Symbol symbol)
        : ASTNode(loc)
        , symbol_(symbol)
    {}
    Identifier(Token tok)
        : ASTNode(tok.loc())
        , symbol_(tok.symbol())
    {}

    Symbol symbol() const { return symbol_; }
    Stream& stream(Stream&) const override;

private:
    Symbol symbol_;
};

class Typeable : public ASTNode {
public:
    Typeable(Loc loc) : ASTNode(loc) {}

    const Type* type() const { return type_; }

protected:
    mutable const Type* type_ = nullptr;

    friend class InferSema;
};

//------------------------------------------------------------------------------

/*
 * paths
 */

class Path : public Typeable {
public:
    class Elem : public Typeable {
    public:
        Elem(const Identifier* id)
            : Typeable(id->loc())
            , identifier_(id)
        {}

        const Identifier* identifier() const { return identifier_.get(); }
        Symbol symbol() const { return identifier()->symbol(); }
        const Decl* decl() const { return decl_; }

        Stream& stream(Stream&) const override;

    private:
        std::unique_ptr<const Identifier> identifier_;
        mutable const Decl* decl_ = nullptr;

        friend class Path;
        friend class Parser;
        friend class InferSema;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    Path(Loc loc, bool global, Elems&& elems)
        : Typeable(loc)
        , global_(global)
        , elems_(std::move(elems))
    {}
    Path(const Identifier* id)
        : Path(id->loc(), false, Elems())
    {
        elems_.emplace_back(new Elem(id));
    }

    bool is_global() const { return global_; }
    const Elems& elems() const { return elems_; }
    const Elem* elem(size_t i) const { return elems_[i].get(); }
    size_t num_elems() const { return elems_.size(); }
    const Decl* decl() const { return elems().back()->decl(); }

    void bind(NameSema&) const;
    const Type* infer(InferSema&) const;
    void check(TypeSema&) const;

    Stream& stream(Stream&) const override;

private:
    bool global_;
    Elems elems_;
};

//------------------------------------------------------------------------------

/*
 * AST types
 */

class ASTType : public Typeable {
public:
    ASTType(Loc loc)
        : Typeable(loc)
    {}

    virtual void bind(NameSema&) const = 0;

private:
    virtual const Type* infer(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(Loc loc)
        : ASTType(loc)
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
};

class PrimASTType : public ASTType {
public:
    enum Tag {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    PrimASTType(Loc loc, Tag tag)
        : ASTType(loc)
        , tag_(tag)
    {}

    Tag tag() const { return tag_; }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
};

class PtrASTType : public ASTType {
public:
    enum Tag { Borrowed, Mut, Owned };

    PtrASTType(Loc loc, Tag tag, int addr_space, const ASTType* referenced_ast_type)
        : ASTType(loc)
        , tag_(tag)
        , addr_space_(addr_space)
        , referenced_ast_type_(referenced_ast_type)
    {}

    Tag tag() const { return tag_; }
    std::string prefix() const;
    const ASTType* referenced_ast_type() const { return referenced_ast_type_.get(); }
    int addr_space() const { return addr_space_; }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    int addr_space_;
    std::unique_ptr<const ASTType> referenced_ast_type_;
};

class ArrayASTType : public ASTType {
public:
    ArrayASTType(Loc loc, const ASTType* elem_ast_type)
        : ASTType(loc)
        , elem_ast_type_(elem_ast_type)
    {}

    const ASTType* elem_ast_type() const { return elem_ast_type_.get(); }

protected:
    std::unique_ptr<const ASTType> elem_ast_type_;
};

class IndefiniteArrayASTType : public ArrayASTType {
public:
    IndefiniteArrayASTType(Loc loc, const ASTType* elem_ast_type)
        : ArrayASTType(loc, elem_ast_type)
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    DefiniteArrayASTType(Loc loc, const ASTType* elem_ast_type, uint64_t dim)
        : ArrayASTType(loc, elem_ast_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    uint64_t dim_;
};

class CompoundASTType : public ASTType {
public:
    CompoundASTType(Loc loc, ASTTypes&& ast_type_args)
        : ASTType(loc)
        , ast_type_args_(std::move(ast_type_args))
    {}

    size_t num_ast_type_args() const { return ast_type_args_.size(); }
    ASTTypeArgs ast_type_args() const { return ast_type_args_; }
    const ASTType* ast_type_arg(size_t i) const { return ast_type_args_[i].get(); }

protected:
    ASTTypes ast_type_args_;
    mutable std::vector<const Type*> type_args_;
};

class TupleASTType : public CompoundASTType {
public:
    TupleASTType(Loc loc, ASTTypes&& ast_type_args)
        : CompoundASTType(loc, std::move(ast_type_args))
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
};

class ASTTypeApp : public CompoundASTType {
public:
    ASTTypeApp(Loc loc, const Path* path, ASTTypes&& ast_type_args)
        : CompoundASTType(loc, std::move(ast_type_args))
        , path_(path)
    {}

    ASTTypeApp(Loc loc, const Path* path)
        : ASTTypeApp(loc, path, ASTTypes())
    {}

    const Path* path() const { return path_.get(); }
    const Identifier* identifier() const { return path()->elems().back()->identifier(); }
    Symbol symbol() const { return identifier()->symbol(); }
    const Decl* decl() const { return path()->decl(); }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Path> path_;
};

class FnASTType : public ASTTypeParamList, public CompoundASTType {
public:
    FnASTType(Loc loc, ASTTypeParams&& ast_type_params, ASTTypes&& ast_type_args)
        : ASTTypeParamList(std::move(ast_type_params))
        , CompoundASTType(loc, std::move(ast_type_args))
    {}

    FnASTType(Loc loc, ASTTypes&& ast_type_args = ASTTypes())
        : ASTTypeParamList(ASTTypeParams())
        , CompoundASTType(loc, std::move(ast_type_args))
    {}

    const FnASTType* ret_fn_ast_type() const;

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
};

class Typeof : public ASTType {
public:
    Typeof(Loc loc, const Expr* expr)
        : ASTType(loc)
        , expr_(dock(expr_, expr))
    {}

    const Expr* expr() const { return expr_.get(); }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> expr_;
};

//------------------------------------------------------------------------------

/*
 * declarations
 */

/// Base class for all entities which have a @p symbol_.
class Decl : public Typeable {
public:
    enum Tag {
        NoDecl,
        NamedDecl,     ///< Just has an @p Identifier.
        TypeableDecl,  ///< Must have a @p Type assigned.
        TypeDecl,      ///< Represents a type definition like @p StructDecl or @p Typedef.
        ValueDecl,     ///< Represents something which is a value at run time.
    };

    /// General constructor.
    Decl(Tag tag, Loc loc, bool mut, const Identifier* id, const ASTType* ast_type)
        : Typeable(loc)
        , tag_(tag)
        , identifier_(id)
        , ast_type_(ast_type)
        , mut_(mut)
        , written_(false)
    {}
    /// @p NoDecl.
    Decl(Loc loc)
        : Decl(NoDecl, loc, false, nullptr, nullptr)
    {}
    /// @p TypeableDecl, @p TypeDecl or @p ValueDecl.
    Decl(Tag tag, Loc loc, const Identifier* id)
        : Decl(tag, loc, false, id, nullptr)
    {}
    /// @p ValueDecl.
    Decl(Loc loc, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(ValueDecl, loc, mut, id, ast_type)
    {}

    // tag
    Tag tag() const { return tag_; }
    bool is_no_decl() const { return tag() == NoDecl; }
    bool is_named_decl() const { return tag() == NamedDecl; }
    bool is_type_decl() const { return tag() == TypeDecl; }
    bool is_value_decl() const { return tag() == ValueDecl; }

    // identifier
    const Identifier* identifier() const { assert(!is_no_decl()); return identifier_.get(); }
    Symbol symbol() const { assert(!is_no_decl()); return identifier_->symbol(); }
    bool is_anonymous() const { assert(!is_no_decl()); return symbol() == Symbol() || symbol().c_str()[0] == '<'; }
    size_t depth() const { assert(!is_no_decl()); return depth_; }
    const Decl* shadows() const { assert(!is_no_decl()); return shadows_; }
    thorin::Debug debug() const { return {symbol().c_str(), loc().filename(), loc().front_line(), loc().front_col(), loc().back_line(), loc().back_col()}; }

    // ValueDecl
    const ASTType* ast_type() const { assert(is_value_decl()); return ast_type_.get(); } ///< Original @p ASTType.
    bool is_mut() const { assert(is_value_decl()); return mut_; }
    bool is_written() const { assert(is_value_decl()); return written_; }
    void write() const { assert(is_value_decl()); written_ = true; }
    const thorin::Def* def() const { return def_; }

private:
    Tag tag_;
    std::unique_ptr<const Identifier> identifier_;
    std::unique_ptr<const ASTType> ast_type_;

protected:
    mutable const thorin::Def* def_ = nullptr;
    mutable const Decl* shadows_;
    mutable unsigned depth_   : 24;
    unsigned mut_             :  1;
    mutable unsigned written_ :  1;

    friend class CodeGen;
    friend class NameSema;
};

/// Base class for all values which may be mutated within a function.
class LocalDecl : public Decl {
public:
    LocalDecl(Loc loc, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(loc, mut, id, ast_type)
    {}
    LocalDecl(Loc loc, const Identifier* id, const ASTType* ast_type)
        : LocalDecl(loc, /*mut*/ false, id, ast_type)
    {}

    const Fn* fn() const { return fn_; }
    void take_address() const { is_address_taken_ = true; }
    void emit(CodeGen&, const thorin::Def*) const;
    void bind(NameSema&) const;

    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const;
    void check(TypeSema&) const;

protected:
    mutable const Fn* fn_;
    mutable bool is_address_taken_ = false;

    friend class CodeGen;
    friend class InferSema;
    friend class TypeSema;
};

//------------------------------------------------------------------------------

/*
 * parameters and Fn
 */

class ASTTypeParam : public Decl {
public:
    ASTTypeParam(Loc loc, const Identifier* id, ASTTypes&& bounds)
        : Decl(TypeDecl, loc, id)
        , bounds_(std::move(bounds))
    {}

    size_t num_bounds() const { return bounds().size(); }
    const ASTTypes& bounds() const { return bounds_; }
    int lambda_depth() const { return lambda_depth_; }
    const Var* var() const { return type()->as<Var>(); }

    void bind(NameSema&) const;
    const Var* check(TypeSema&) const;
    Stream& stream(Stream&) const override;

private:
    const Var* infer(InferSema&) const;

    ASTTypes bounds_;
    mutable int lambda_depth_ = -1;

    friend class ASTTypeApp;
    friend class ASTTypeParamList;
    friend class InferSema;
};

class Param : public LocalDecl {
public:
    Param(Loc loc, bool mut, const Identifier* id, const ASTType* ast_type)
        : LocalDecl(loc, mut, id, ast_type)
    {}
    Param(Loc loc, const Identifier* id, const ASTType* ast_type)
        : Param(loc, /*mut*/ false, id, ast_type)
    {}

    Stream& stream(Stream&) const override;
};

class Fn : public ASTTypeParamList {
public:
    Fn(const Expr* filter, ASTTypeParams&& ast_type_params, Params&& params, const Expr* body)
        : ASTTypeParamList(std::move(ast_type_params))
        , filter_(dock(filter_, filter))
        , params_(std::move(params))
        , body_(dock(body_, body))
    {}

    const Expr* filter() const { return filter_.get(); }
    const Param* param(size_t i) const { return params_[i].get(); }
    ArrayRef<std::unique_ptr<const Param>> params() const { return params_; }
    size_t num_params() const { return params_.size(); }
    const Expr* body() const { return body_.get(); }
    thorin::Lam* lam() const { return lam_; }
    const thorin::Def* ret_param() const { return ret_param_; }
    const thorin::Def* frame() const { return frame_; }
    Stream& stream_params(Stream& p, bool returning) const;
    void fn_bind(NameSema&) const;
    const Type* check_body(TypeSema&) const;
    thorin::Lam* fn_emit_head(CodeGen&, Loc) const;
    void fn_emit_body(CodeGen&, Loc) const;

    virtual const FnType* fn_type() const = 0;
    virtual Symbol fn_symbol() const = 0;

protected:
    std::unique_ptr<const Expr> filter_;
    Params params_;
    mutable thorin::Lam* lam_ = nullptr;
    mutable const thorin::Def* ret_param_ = nullptr;
    mutable const thorin::Def* frame_ = nullptr;

private:
    std::unique_ptr<const Expr> body_;
};

//------------------------------------------------------------------------------

/*
 * items
 */

class Item : public Decl {
public:
    /// @p NoDecl.
    Item(Loc loc, Visibility vis)
        : Decl(loc)
        , visibility_(vis)
    {}

    /// @p TypeableDecl, @p TypeDecl or @p ValueDecl.
    Item(Tag tag, Loc loc, Visibility vis, const Identifier* id)
        : Decl(tag, loc, id)
        , visibility_(vis)
    {}

    /// @p ValueDecl.
    Item(Loc loc, Visibility vis, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(ValueDecl, loc, mut, id, ast_type)
        , visibility_(vis)
    {}

    Visibility visibility() const { return visibility_; }
    virtual void bind(NameSema&) const = 0;
    virtual void emit_head(CodeGen&) const {};
    virtual void emit(CodeGen&) const = 0;

private:
    virtual void infer(InferSema&) const = 0;
    virtual const Type* infer_head(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    Visibility visibility_;

    friend class CodeGen;
    friend class InferSema;
    friend class TypeSema;
};

class TypeDeclItem : public Item, public ASTTypeParamList {
public:
    TypeDeclItem(Loc loc, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params)
        : Item(TypeDecl, loc,  vis, id)
        , ASTTypeParamList(std::move(ast_type_params))
    {}
};

class ValueItem : public Item {
public:
    ValueItem(Loc loc, Visibility vis, bool mut, const Identifier* id, const ASTType* ast_type)
        : Item(loc, vis, mut, id, ast_type)
    {}
};

class Module : public TypeDeclItem {
public:
    Module(Loc loc, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params, Items&& items)
        : TypeDeclItem(loc, vis, id, std::move(ast_type_params))
        , items_(std::move(items))
    {}

    Module(const char* first_file_name, Items&& items = Items())
        : Module(items.empty() ? Loc(first_file_name, 1, 1) : Loc(items.front()->loc(), items.back()->loc()),
                 Visibility::Pub, nullptr, ASTTypeParams(), std::move(items))
    {}

    const Items& items() const { return items_; }
    const Symbol2Item& symbol2item() const { return symbol2item_; }

    void bind(NameSema&) const override;
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    Items items_;
    mutable Symbol2Item symbol2item_;
};

class ModuleDecl : public TypeDeclItem {
public:
    ModuleDecl(Loc loc, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params)
        : TypeDeclItem(loc, vis, id, std::move(ast_type_params))
    {}

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;
};

class ExternBlock : public Item {
public:
    ExternBlock(Loc loc, Visibility vis, Symbol abi, FnDecls&& fn_decls)
        : Item(loc, vis)
        , abi_(abi)
        , fn_decls_(std::move(fn_decls))
    {}

    Symbol abi() const { return abi_; }
    const FnDecls& fn_decls() const { return fn_decls_; }

    void bind(NameSema&) const override;
    void emit_head(CodeGen&) const override;
    void emit(CodeGen&) const override {}
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbol abi_;
    FnDecls fn_decls_;
};

class Typedef : public TypeDeclItem {
public:
    Typedef(Loc loc, Visibility vis, const Identifier* id,
            ASTTypeParams&& ast_type_params, const ASTType* ast_type)
        : TypeDeclItem(loc, vis, id, std::move(ast_type_params))
        , ast_type_(ast_type)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const ASTType> ast_type_;
};

class FieldDecl : public Decl {
public:
    FieldDecl(Loc loc, size_t index, Visibility vis, const Identifier* id, const ASTType* ast_type)
        : Decl(TypeableDecl, loc, id)
        , index_(index)
        , visibility_(vis)
        , ast_type_(std::move(ast_type))
    {}

    uint32_t index() const { return index_; }
    const ASTType* ast_type() const { return ast_type_.get(); }
    Visibility visibility() const { return  visibility_; }

    void bind(NameSema&) const;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const;
    void check(TypeSema&) const;

    uint32_t index_;
    Visibility visibility_;
    std::unique_ptr<const ASTType> ast_type_;

    friend class InferSema;
    friend class TypeSema;
};

class StructDecl : public TypeDeclItem {
public:
    StructDecl(Loc loc, Visibility vis, const Identifier* id,
               ASTTypeParams&& ast_type_params, FieldDecls&& field_decls)
        : TypeDeclItem(loc, vis, id, std::move(ast_type_params))
        , field_decls_(std::move(field_decls))
    {}

    size_t num_field_decls() const { return field_decls_.size(); }
    const FieldDecls& field_decls() const { return field_decls_; }
    const FieldTable& field_table() const { return field_table_; }
    const FieldDecl* field_decl(size_t i) const { return field_decls_[i].get(); }
    std::optional<const FieldDecl*> field_decl(Symbol symbol) const { return field_table_.lookup(symbol); }
    std::optional<const FieldDecl*> field_decl(const Identifier* ident) const { return field_decl(ident->symbol()); }
    const StructType* struct_type() const { return type_->as<StructType>(); }

    void bind(NameSema&) const override;
    void emit_head(CodeGen&) const override;
    void emit(CodeGen&) const override {}
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    FieldDecls field_decls_;
    mutable FieldTable field_table_;
};

class OptionDecl : public Decl {
public:
    OptionDecl(Loc loc, size_t index, const Identifier* id, ASTTypes args)
        : Decl(ValueDecl, loc, id)
        , index_(index)
        , args_(std::move(args))
    {}

    uint32_t index() const { return index_; }
    size_t num_args() const { return args_.size(); }
    const ASTTypes& args() const { return args_; }
    const ASTType* arg(size_t i) const { return args_[i].get(); }
    const EnumDecl* enum_decl() const { return enum_decl_; }

    void bind(NameSema&) const;
    void emit(CodeGen&) const;
    Stream& stream(Stream&) const override;

    const thorin::Def* variant_type(CodeGen&) const;

private:
    const Type* infer(InferSema&) const;
    void check(TypeSema&) const;

    uint32_t index_;
    ASTTypes args_;

    mutable const EnumDecl* enum_decl_;

    friend class EnumDecl;
    friend class InferSema;
    friend class TypeSema;
};

class EnumDecl : public TypeDeclItem {
public:
    EnumDecl(Loc loc, Visibility vis, const Identifier* id,
             ASTTypeParams&& ast_type_params, OptionDecls&& option_decls)
        : TypeDeclItem(loc, vis, id, std::move(ast_type_params))
        , option_decls_(std::move(option_decls))
    {
        for (auto& option : option_decls_)
            simple_ &= option->num_args() == 0;
    }

    bool is_simple() const { return simple_; }
    size_t num_option_decls() const { return option_decls_.size(); }
    const OptionDecls& option_decls() const { return option_decls_; }
    const OptionDecl* option_decl(size_t i) const { return option_decls_[i].get(); }
    std::optional<const OptionDecl*> option_decl(Symbol symbol) const { return option_table_.lookup(symbol); }
    const EnumType* enum_type() const { return type_->as<EnumType>(); }

    void bind(NameSema&) const override;
    void emit_head(CodeGen&) const override;
    void emit(CodeGen&) const override {}
    Stream& stream(Stream& os) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    bool simple_ = true;
    OptionDecls option_decls_;
    mutable OptionTable option_table_;
};

class StaticItem : public ValueItem {
public:
    StaticItem(Loc loc, Visibility vis, bool mut, const Identifier* id,
               const ASTType* ast_type, const Expr* init)
        : ValueItem(loc, vis, mut, id, std::move(ast_type))
        , init_(dock(init_, init))
    {}

    const Expr* init() const { return init_.get(); }

    void bind(NameSema&) const override;
    void emit_head(CodeGen&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> init_;
};

class FnDecl : public ValueItem, public Fn {
public:
    FnDecl(Loc loc, Visibility vis, bool is_extern, Symbol abi, const Expr* filter, Symbol export_name,
           const Identifier* id, ASTTypeParams&& ast_type_params, Params&& params, const Expr* body)
        : ValueItem(loc, vis, /*mut*/ false, id, /*ast_type*/ nullptr)
        , Fn(filter, std::move(ast_type_params), std::move(params), body)
        , abi_(abi)
        , export_name_(export_name)
        , is_extern_(is_extern)
    {}

    bool is_extern() const { return is_extern_; }
    Symbol abi() const { return abi_; }

    const FnType* fn_type() const override {
        auto t = type();
        while (auto lambda = t->isa<Lambda>())
            t = lambda->body();
        return t->as<FnType>();
    }
    Symbol fn_symbol() const override { return export_name_ != "" ? export_name_ : identifier()->symbol(); }

    void bind(NameSema&) const override;
    void emit_head(CodeGen&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbol abi_;
    Symbol export_name_;
    bool is_extern_ = false;
};

class TraitDecl : public Item, public ASTTypeParamList {
public:
    TraitDecl(Loc loc, Visibility vis, const Identifier* id,
              ASTTypeParams&& ast_type_params, ASTTypeApps&& super_traits, FnDecls&& methods)
        : Item(TypeDecl, loc, vis, id)
        , ASTTypeParamList(std::move(ast_type_params))
        , super_traits_(std::move(super_traits))
        , methods_(std::move(methods))
    {}

    const ASTTypeApps& super_traits() const { return super_traits_; }
    const FnDecls& methods() const { return methods_; }
    const MethodTable& method_table() const { return method_table_; }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    ASTTypeApps super_traits_;
    FnDecls methods_;
    mutable MethodTable method_table_;
};

class ImplItem : public Item, public ASTTypeParamList {
public:
    ImplItem(Loc loc, Visibility vis, ASTTypeParams&& ast_type_params,
             const ASTType* trait, const ASTType* ast_type, FnDecls&& methods)
        : Item(loc, vis)
        , ASTTypeParamList(std::move(ast_type_params))
        , trait_(std::move(trait))
        , ast_type_(std::move(ast_type))
        , methods_(std::move(methods))
    {}

    /// May be nullptr as trait is optional.
    const ASTType* trait() const { return trait_.get(); }
    const ASTType* ast_type() const { return ast_type_.get(); }
    const FnDecls& methods() const { return methods_; }
    const FnDecl* method(size_t i) const { return methods_[i].get(); }
    size_t num_methods() const { return methods_.size(); }
    const thorin::Def* def() const { return def_; }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    const Type* infer_head(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const ASTType> trait_;
    std::unique_ptr<const ASTType> ast_type_;
    FnDecls methods_;
    mutable const thorin::Def* def_;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public Typeable {
public:
    Expr(Loc loc)
        : Typeable(loc)
    {}

    virtual ~Expr() { assert(back_ref_ != nullptr); }

    const Expr* skip_rvalue() const;

    virtual void write() const {}
    virtual bool has_side_effect() const { return false; }
    virtual void take_address() const {}
    virtual void bind(NameSema&) const = 0;
    virtual const thorin::Def* lemit(CodeGen&) const;
    virtual const thorin::Def* remit(CodeGen&) const;
    virtual void emit_branch(CodeGen&, thorin::Lam*, thorin::Lam*) const;

private:
    virtual const Type* infer(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

protected:
    /**
     * A back reference to the @p std::unique_ptr which owns this @p Expr.
     * This means that the address is @em not supposed to be changed in the future.
     * For this reason, @p Exprs is a <tt> std::deque<std::unique_ptr<const Expr>> </tt> and @em not a @c std::vector.
     */
    mutable std::unique_ptr<const Expr>* back_ref_ = nullptr;

    friend const Expr* dock(std::unique_ptr<const Expr>& dst, const Expr* src) {
        if (src) {
            assert(src->back_ref_ == nullptr);
            src->back_ref_ = &dst;
        }
        return src;
    }

    template<class T, class...Args>
    friend const T* interlope(const Expr* expr, Args&&... args);
    friend const PrefixExpr* replace_rvalue_by_addrof(const RValueExpr* expr);
    friend class Args;
    friend class CodeGen;
    friend class InferSema;
    friend class TypeSema;
};

/**
 * Creates a new @p ASTNode @p T using @p args as constructor arguments
 * while @p expr gets released from its @p Expr::back_ref_ and the newly created node is @p expr's new owner.
 * This means that @p expr also occurs within @p args.
 */
template<class T, class...Args>
const T* interlope(const Expr* expr, Args&&... args) {
    auto parent = expr->back_ref_;
    parent->release();
    expr->back_ref_ = nullptr;
    auto new_expr = new T(std::forward<Args>(args)...);
    parent->reset(new_expr);
    new_expr->back_ref_ = parent;
    return new_expr;
}

const PrefixExpr* replace_rvalue_by_addrof(const RValueExpr* rvalue);

/// Use as mixin for anything which uses args: (expr_1, ..., expr_n)
class Args {
public:
    Args(Exprs&& args)
        : args_(std::move(args))
    {
        for (auto& arg : args_)
            arg->back_ref_ = &arg;
    }

    const Exprs& args() const { return args_; }
    const Expr* arg(size_t i) const { assert(i < args_.size()); return args_[i].get(); }
    size_t num_args() const { return args_.size(); }
    Stream& stream_args(Stream& p) const;

protected:
    Exprs args_;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(Loc loc)
        : Expr(loc)
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class LiteralExpr : public Expr {
public:
    enum Tag {
#define IMPALA_LIT(itype, atype) LIT_##itype = Token::LIT_##itype,
#include "impala/tokenlist.h"
        LIT_bool,
    };

    LiteralExpr(Loc loc, Tag tag, uint64_t val)
        : Expr(loc)
        , tag_(tag)
        , val_(val)
    {}

    Tag tag() const { return tag_; }
    template<class T = uint64_t> T get() const { return thorin::bitcast<T>(val_); }
    PrimTypeTag literal2type() const;

    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    uint64_t val_;
};

class CharExpr : public Expr {
public:
    CharExpr(Loc loc, Symbol symbol, char value)
        : Expr(loc)
        , symbol_(symbol)
        , value_(value)
    {}

    Symbol symbol() const { return symbol_; }
    char value() const { return value_; }

    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbol symbol_;
    char value_;
};

class StrExpr : public Expr {
public:
    StrExpr(Loc loc, Symbols&& symbols, std::vector<char>&& values)
        : Expr(loc)
        , symbols_(std::move(symbols))
        , values_(std::move(values))
    {}

    const Symbols& symbols() const { return symbols_; }
    const std::vector<char>& values() const { return values_; }

    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbols symbols_;
    mutable std::vector<char> values_;
};

class FnExpr : public Expr, public Fn {
public:
    FnExpr(Loc loc, const Expr* filter, Params&& params, const Expr* body)
        : Expr(loc)
        , Fn(filter, ASTTypeParams(), std::move(params), body)
    {}

    const FnType* fn_type() const override { return type()->as<FnType>(); }
    Symbol fn_symbol() const override { return Symbol("lambda"); }
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
};

class PathExpr : public Expr {
public:
    PathExpr(const Path* path)
        : Expr(path->loc())
        , path_(path)
    {}
    PathExpr(const Identifier* identifier)
        : PathExpr(new Path(identifier))
    {}

    const Path* path() const { return path_.get(); }
    const Decl* value_decl() const {
        return path_->decl() && path_->decl()->is_value_decl() ? path_->decl() : nullptr;
    }

    void write() const override;
    void take_address() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;

    std::unique_ptr<const Path> path_;
};

class PrefixExpr : public Expr {
public:
    enum Tag {
#define IMPALA_PREFIX(tok, str) tok = Token:: tok,
#include "impala/tokenlist.h"
        MUT
    };

    PrefixExpr(Loc loc, Tag tag, const Expr* rhs)
        : Expr(loc)
        , tag_(tag)
        , rhs_(dock(rhs_, rhs))
    {}

    static const PrefixExpr* create(const Expr* rhs, const Tag tag) {
        return interlope<PrefixExpr>(rhs, rhs->loc(), tag, rhs);
    }
    static const PrefixExpr* create_deref(const Expr* rhs) { return create(rhs, MUL); }
    static const PrefixExpr* create_addrof(const Expr* rhs);

    const Expr* rhs() const { return rhs_.get(); }
    Tag tag() const { return tag_; }

    void write() const override;
    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    std::unique_ptr<const Expr> rhs_;
};

class InfixExpr : public Expr {
public:
    enum Tag {
#define IMPALA_INFIX_ASGN(tok, str)       tok = Token:: tok,
#define IMPALA_INFIX(     tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    InfixExpr(Loc loc, const Expr* lhs, Tag tag, const Expr* rhs)
        : Expr(loc)
        , tag_(tag)
        , lhs_(dock(lhs_, lhs))
        , rhs_(dock(rhs_, rhs))
    {}

    Tag tag() const { return tag_; }
    const Expr* lhs() const { return lhs_.get(); }
    const Expr* rhs() const { return rhs_.get(); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_branch(CodeGen&, thorin::Lam*, thorin::Lam*) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    std::unique_ptr<const Expr> lhs_;
    std::unique_ptr<const Expr> rhs_;
};

/**
 * Just for expr++ and expr--.
 * For indexing/function calls use @p MapExpr.
 */
class PostfixExpr : public Expr {
public:
    enum Tag {
        INC = Token::INC,
        DEC = Token::DEC
    };

    PostfixExpr(Loc loc, const Expr* lhs, Tag tag)
        : Expr(loc)
        , tag_(tag)
        , lhs_(dock(lhs_, lhs))
    {}

    Tag tag() const { return tag_; }
    const Expr* lhs() const { return lhs_.get(); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    std::unique_ptr<const Expr> lhs_;
};

class FieldExpr : public Expr {
public:
    FieldExpr(Loc loc, const Expr* lhs, const Identifier* id)
        : Expr(loc)
        , lhs_(dock(lhs_, lhs))
        , identifier_(id)
    {}

    const Expr* lhs() const { return lhs_.get(); }
    const Identifier* identifier() const { return identifier_.get(); }
    Symbol symbol() const { return identifier()->symbol(); }
    const FieldDecl* field_decl() const { return field_decl_; }
    uint32_t index() const { return field_decl()->index(); }

    void write() const override;
    void take_address() const override;
    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;
    std::unique_ptr<const Identifier> identifier_;
    mutable const FieldDecl* field_decl_ = nullptr;
};

class CastExpr : public Expr {
public:
    CastExpr(Loc loc, const Expr* src)
        : Expr(loc)
        , src_(dock(src_, src))
    {}

    const Expr* src() const { return src_.get(); }
    void write() const override;

private:
    const thorin::Def* remit(CodeGen&) const override;

protected:
    void check(TypeSema&) const override;
    std::unique_ptr<const Expr> src_;
};

class ExplicitCastExpr : public CastExpr {
public:
    ExplicitCastExpr(Loc loc, const Expr* src, const ASTType* ast_type)
        : CastExpr(loc, src)
        , ast_type_(ast_type)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const ASTType> ast_type_;
};

class ImplicitCastExpr : public CastExpr {
public:
    ImplicitCastExpr(const Expr* src, const Type* type)
        : CastExpr(src->loc(), src)
    {
        type_ = type;
    }

    static const ImplicitCastExpr* create(const Expr* src, const Type* type) {
        return interlope<ImplicitCastExpr>(src, src, type);
    }

    void bind(NameSema&) const override { THORIN_UNREACHABLE; }
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
};

class RValueExpr : public CastExpr {
public:
    RValueExpr(const Expr* src)
        : CastExpr(src->loc(), src)
    {}

    static const RValueExpr* create(const Expr* src) {
        return interlope<RValueExpr>(src, src);
    }

    bool has_side_effect() const override;

    void bind(NameSema&) const override { THORIN_UNREACHABLE; }
    Stream& stream(Stream&) const override;

private:
    void check(TypeSema&) const override;
    const Type* infer(InferSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class DefiniteArrayExpr : public Expr, public Args {
public:
    DefiniteArrayExpr(Loc loc, Exprs&& args)
        : Expr(loc)
        , Args(std::move(args))
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    RepeatedDefiniteArrayExpr(Loc loc, const Expr* value, uint64_t count)
        : Expr(loc)
        , value_(dock(value_, value))
        , count_(count)
    {}

    const Expr* value() const { return value_.get(); }
    uint64_t count() const { return count_; }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> value_;
    uint64_t count_;
};

class IndefiniteArrayExpr : public Expr {
public:
    IndefiniteArrayExpr(Loc loc, const Expr* dim, const ASTType* elem_ast_type)
        : Expr(loc)
        , dim_(dock(dim_, dim))
        , elem_ast_type_(elem_ast_type)
    {}

    const Expr* dim() const { return dim_.get(); }
    const ASTType* elem_ast_type() const { return elem_ast_type_.get(); }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> dim_;
    std::unique_ptr<const ASTType> elem_ast_type_;
};

class TupleExpr : public Expr, public Args {
public:
    TupleExpr(Loc loc, Exprs&& args)
        : Expr(loc)
        , Args(std::move(args))
    {}

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class StructExpr : public Expr {
public:
    class Elem : public ASTNode {
    public:
        Elem(Loc loc, const Identifier* id, const Expr* expr)
            : ASTNode(loc)
            , identifier_(id)
            , expr_(dock(expr_, expr))
        {}

        const Identifier* identifier() const { return identifier_.get(); }
        Symbol symbol() const { return identifier()->symbol(); }
        const Expr* expr() const { return expr_.get(); }
        const FieldDecl* field_decl() const { return field_decl_; }

        Stream& stream(Stream&) const override;

    private:
        std::unique_ptr<const Identifier> identifier_;
        std::unique_ptr<const Expr> expr_;
        mutable const FieldDecl* field_decl_ = nullptr;

        friend class StructExpr;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    StructExpr(Loc loc, const ASTTypeApp* ast_type_app, Elems&& elems)
        : Expr(loc)
        , ast_type_app_(ast_type_app)
        , elems_(std::move(elems))
    {}

    const ASTTypeApp* ast_type_app() const { return ast_type_app_.get(); }
    const Elem* elem(size_t i) const { return elems_[i].get(); }
    size_t num_elems() const { return elems_.size(); }
    const Elems& elems() const { return elems_; }

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const ASTTypeApp> ast_type_app_;
    Elems elems_;
};

class TypeAppExpr : public Expr {
public:
    TypeAppExpr(Loc loc, const Expr* lhs, ASTTypes&& ast_type_args)
        : Expr(loc)
        , lhs_(dock(lhs_, lhs))
        , ast_type_args_(std::move(ast_type_args))
    {}

    static const TypeAppExpr* create(const Expr* lhs) {
        return interlope<TypeAppExpr>(lhs, lhs->loc(), lhs, ASTTypes());
    }

    const Expr* lhs() const { return lhs_.get(); }
    const ASTTypes& ast_type_args() const { return ast_type_args_; }
    const ASTType* ast_type_arg(size_t i) const { assert(i < ast_type_args_.size()); return ast_type_args_[i].get(); }
    size_t num_ast_type_args() const { return ast_type_args().size(); }
    Types type_args() const { return type_args_; }
    const Type*& type_arg(size_t i) const { return type_args_[i]; }
    size_t num_type_args() const { return type_args_.size(); }
    Stream& stream_ast_type_args(Stream& p) const;
    Stream& stream_type_args(Stream& p) const;

    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;
    ASTTypes ast_type_args_;
    mutable std::vector<const Type*> type_args_;
};

class MapExpr : public Expr, public Args {
public:
    MapExpr(Loc loc, const Expr* lhs, Exprs&& args)
        : Expr(loc)
        , Args(std::move(args))
        , lhs_(dock(lhs_, lhs))
    {}

    const Expr* lhs() const { return lhs_.get(); }

    void write() const override;
    bool has_side_effect() const override;
    void take_address() const override;
    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;

    friend class CodeGen;
};

class BlockExpr : public Expr {
public:
    BlockExpr(Loc loc, Stmts&& stmts, const Expr* expr)
        : Expr(loc)
        , stmts_(std::move(stmts))
        , expr_(dock(expr_, expr))
    {}
    /// An empty BlockExpr with no @p stmts and an @p EmptyExpr as @p expr.
    BlockExpr(Loc loc)
        : BlockExpr(loc, Stmts(), new EmptyExpr(loc))
    {}

    const Stmts& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_.get(); }
    const Stmt* stmt(size_t i) const { return stmts_[i].get(); }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    const LocalDecls& locals() const { return locals_; }
    void add_local(const LocalDecl* local) const { locals_.push_back(local); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

protected:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Stmts stmts_;
    std::unique_ptr<const Expr> expr_;
    mutable LocalDecls locals_; ///< All @p LocalDecl%s in this @p BlockExpr from top to bottom.
};

class IfExpr : public Expr {
public:
    IfExpr(Loc loc, const Expr* cond, const Expr* then_expr, const Expr* else_expr)
        : Expr(loc)
        , cond_(dock(cond_, cond))
        , then_expr_(dock(then_expr_, then_expr))
        , else_expr_(dock(else_expr_, else_expr))
    {}

    const Expr* cond() const { return cond_.get(); }
    const Expr* then_expr() const { return then_expr_.get(); }
    const Expr* else_expr() const { return else_expr_.get(); }
    bool has_else() const;

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> cond_;
    std::unique_ptr<const Expr> then_expr_;
    std::unique_ptr<const Expr> else_expr_;
};

class MatchExpr : public Expr {
public:
    class Arm : public ASTNode {
    public:
        Arm(Loc loc, const Ptrn* ptrn, const Expr* expr)
            : ASTNode(loc)
            , ptrn_(ptrn)
            , expr_(dock(expr_, expr))
        {}

        const Ptrn* ptrn() const { return ptrn_.get(); }
        const Expr* expr() const { return expr_.get(); }
        Stream& stream(Stream&) const override;

    private:
        std::unique_ptr<const Ptrn> ptrn_;
        std::unique_ptr<const Expr> expr_;
    };

    typedef std::deque<std::unique_ptr<const Arm>> Arms;

    MatchExpr(Loc loc, const Expr* expr, Arms&& arms)
        : Expr(loc)
        , expr_(dock(expr_, expr))
        , arms_(std::move(arms))
    {}

    const Expr* expr() const { return expr_.get(); }
    const Arm* arm(size_t i) const { return arms_[i].get(); }
    const Arms& arms() const { return arms_; }
    size_t num_arms() const { return arms_.size(); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> expr_;
    Arms arms_;
};

class WhileExpr : public Expr {
public:
    WhileExpr(Loc loc, const LocalDecl* continue_decl, const Expr* cond,
              const Expr* body, const LocalDecl* break_decl)
        : Expr(loc)
        , continue_decl_(continue_decl)
        , cond_(dock(cond_, cond))
        , body_(dock(body_, body))
        , break_decl_(break_decl)
    {}

    const Expr* cond() const { return cond_.get(); }
    const BlockExpr* body() const { return body_.get()->as<BlockExpr>(); }
    const LocalDecl* break_decl() const { return break_decl_.get(); }
    const LocalDecl* continue_decl() const { return continue_decl_.get(); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const LocalDecl> continue_decl_;
    std::unique_ptr<const Expr> cond_;
    std::unique_ptr<const Expr> body_;
    std::unique_ptr<const LocalDecl> break_decl_;
};

class ForExpr : public Expr {
public:
    ForExpr(Loc loc, const Expr* fn_expr, const Expr* expr, const LocalDecl* break_decl)
        : Expr(loc)
        , fn_expr_(dock(fn_expr_, fn_expr))
        , expr_(dock(expr_, expr))
        , break_decl_(break_decl)
    {}

    const FnExpr* fn_expr() const { return fn_expr_.get()->as<FnExpr>(); }
    const Expr* expr() const { return expr_.get(); }
    const LocalDecl* break_decl() const { return break_decl_.get(); }

    bool has_side_effect() const override;
    void bind(NameSema&) const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> fn_expr_;
    std::unique_ptr<const Expr> expr_;
    std::unique_ptr<const LocalDecl> break_decl_;
};

//------------------------------------------------------------------------------

/*
 * patterns
 */

class Ptrn : public Typeable {
public:
    Ptrn(Loc loc)
        : Typeable(loc)
    {}

    virtual void bind(NameSema&) const = 0;
    virtual void emit(CodeGen&, const thorin::Def*) const = 0;
    virtual const thorin::Def* emit(CodeGen&) const { return nullptr; }
    virtual const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const = 0;
    virtual bool is_refutable() const = 0;

private:
    virtual const Type* infer(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class TuplePtrn : public Ptrn {
public:
    TuplePtrn(Loc loc, Ptrns&& elems)
        : Ptrn(loc)
        , elems_(std::move(elems))
    {}

    const Ptrns& elems() const { return elems_; }
    const Ptrn* elem(size_t i) const { return elems_[i].get(); }
    size_t num_elems() const { return elems_.size(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const override;
    bool is_refutable() const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    Ptrns elems_;
};

class IdPtrn : public Ptrn {
public:
    IdPtrn(const LocalDecl* local)
        : Ptrn(local->loc())
        , local_(local)
    {}

    const LocalDecl* local() const { return local_.get(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const override;
    bool is_refutable() const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const LocalDecl> local_;
};

class EnumPtrn : public Ptrn {
public:
    EnumPtrn(Loc loc, const Path* path, Ptrns&& args)
        : Ptrn(loc)
        , path_(path)
        , args_(std::move(args))
    {}

    const Path* path() const { return path_.get(); }
    const Ptrns& args() const { return args_; }
    const Ptrn* arg(size_t i) const { return args_[i].get(); }
    size_t num_args() const { return args_.size(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const override;
    bool is_refutable() const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Path> path_;
    Ptrns args_;
};

class LiteralPtrn : public Ptrn {
public:
    LiteralPtrn(const LiteralExpr* literal, bool minus)
        : Ptrn(literal->loc())
        , literal_(dock(literal_, literal))
        , minus_(minus)
    {}

    const LiteralExpr* literal() const { return literal_.get()->as<LiteralExpr>(); }
    bool has_minus() const { return minus_; }

    void bind(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    const thorin::Def* emit(CodeGen&) const override;
    const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const override;
    bool is_refutable() const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> literal_;
    bool minus_;
};

class CharPtrn : public Ptrn {
public:
    CharPtrn(const CharExpr* chr)
        : Ptrn(chr->loc())
        , chr_(dock(chr_, chr))
    {}

    const CharExpr* chr() const { return chr_.get()->as<CharExpr>(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    const thorin::Def* emit(CodeGen&) const override;
    const thorin::Def* emit_cond(CodeGen&, const thorin::Def*) const override;
    bool is_refutable() const override;
    Stream& stream(Stream&) const override;

private:
    const Type* infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> chr_;
};

//------------------------------------------------------------------------------

/*
 * statements
 */

class Stmt : public ASTNode {
public:
    Stmt(Loc loc)
        : ASTNode(loc)
    {}

    virtual void bind(NameSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;

private:
    virtual void infer(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(Loc loc, const Expr* expr)
        : Stmt(loc)
        , expr_(dock(expr_, expr))
    {}

    const Expr* expr() const { return expr_.get(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> expr_;
};

class ItemStmt : public Stmt {
public:
    ItemStmt(Loc loc, const Item* item)
        : Stmt(loc)
        , item_(item)
    {}

    const Item* item() const { return item_.get(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Item> item_;
};

class LetStmt : public Stmt {
public:
    LetStmt(Loc loc, const Ptrn* ptrn, const Expr* init)
        : Stmt(loc)
        , ptrn_(ptrn)
        , init_(dock(init_, init))
    {}

    const Ptrn* ptrn() const { return ptrn_.get(); }
    const Expr* init() const { return init_.get(); }

    void bind(NameSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    void infer(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Ptrn> ptrn_;
    std::unique_ptr<const Expr> init_;
};

class AsmStmt : public Stmt {
public:
    class Elem : public ASTNode {
    public:
        Elem(Loc loc, std::string&& constraint, const Expr* expr)
            : ASTNode(loc)
            , constraint_(std::move(constraint))
            , expr_(dock(expr_, expr))
        {}

        const std::string& constraint() const { return constraint_; }
        const Expr* expr() const { return expr_.get(); }

        Stream& stream(Stream&) const override;

    private:
        std::string constraint_;
        std::unique_ptr<const Expr> expr_;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    AsmStmt(Loc loc, std::string&& asm_template, Elems&& outputs, Elems&& inputs,
            Strings&& clobbers, Strings&& options)
        : Stmt(loc)
        , asm_template_(std::move(asm_template))
        , outputs_(std::move(outputs))
        , inputs_(std::move(inputs))
        , clobbers_(std::move(clobbers))
        , options_(std::move(options))
    {}

    const std::string& asm_template() const { return asm_template_; }
    const Elems& outputs() const { return outputs_; }
    const Elems&  inputs() const { return  inputs_; }
    const Elem* output(size_t i) const { return outputs_[i].get(); }
    const Elem*  input(size_t i) const { return  inputs_[i].get(); }
    size_t num_outputs() const { return outputs().size(); }
    size_t  num_inputs() const { return  inputs().size(); }
    ArrayRef<std::string> clobbers() const { return clobbers_; }
    ArrayRef<std::string> options() const { return options_; }

    Array<std::string> output_constraints() const {
        Array<std::string> result(num_outputs());
        for (size_t i = 0, e = result.size(); i != e; ++i)
            result[i] = output(i)->constraint();
        return result;
    }

    Array<std::string> input_constraints() const {
        Array<std::string> result(num_inputs());
        for (size_t i = 0, e = result.size(); i != e; ++i)
            result[i] = input(i)->constraint();
        return result;
    }

    void bind(NameSema&) const override;
    void infer(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;
    Stream& stream(Stream&) const override;

private:
    std::string asm_template_;
    Elems outputs_;
    Elems inputs_;
    std::vector<std::string> clobbers_;
    std::vector<std::string> options_;
};

//------------------------------------------------------------------------------

}

#endif
