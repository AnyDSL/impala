#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/location.h"
#include "thorin/util/types.h"

#include "impala/impala.h"
#include "impala/symbol.h"
#include "impala/token.h"
#include "impala/sema/type.h"

namespace thorin {

class JumpTarget;
class Continuation;
class Param;

}

namespace impala {

class ASTType;
class ASTTypeApp;
class ASTTypeParam;
class Decl;
class Expr;
class FieldDecl;
class Fn;
class FnDecl;
class LocalDecl;
class Param;
class Ptrn;
class Stmt;

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
typedef std::vector<std::unique_ptr<const FnDecl>> FnDecls;
typedef std::vector<std::unique_ptr<const Param>> Params;
typedef std::vector<std::unique_ptr<const Stmt>> Stmts;
typedef std::vector<char> Chars;
typedef thorin::HashMap<Symbol, const FieldDecl*> FieldTable;
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
MyExpr(Location location, ..., const Expr* expr, ...)
    : Expr(location)
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

private:
    int visibility_;
};

/// Mixin for all entities that have a type assigned.
class Typeable {
public:
    const Type* type() const { return type_; }

protected:
    mutable const Type* type_ = nullptr;

    friend class InferSema;
};

/// Mixin for all entities which have a list of \p TypeParam%s: [T1, T2 : A + B[...], ...].
class ASTTypeParamList {
public:
    ASTTypeParamList(ASTTypeParams&& ast_type_params)
        : ast_type_params_(std::move(ast_type_params))
    {}

    size_t num_ast_type_params() const { return ast_type_params_.size(); }
    const ASTTypeParam* ast_type_param(size_t i) const { return ast_type_params_[i].get(); }
    const ASTTypeParams& ast_type_params() const { return ast_type_params_; }
    std::ostream& stream_ast_type_params(std::ostream&) const;

protected:
    void check_ast_type_params(NameSema&) const;
    void check_ast_type_params(InferSema&) const;
    void check_ast_type_params(TypeSema&) const;

    ASTTypeParams ast_type_params_;
};

//------------------------------------------------------------------------------

class ASTNode : public thorin::MagicCast<ASTNode>, public thorin::Streamable  {
public:
    ASTNode() = delete;
    ASTNode(const ASTNode&) = delete;
    ASTNode(ASTNode&&) = delete;

    ASTNode(Location location)
        : location_(location)
    {}

#ifndef NDEBUG
    virtual ~ASTNode() { assert(location_.is_set()); }
#endif

    Location location() const { return location_; }

private:
    Location location_;
};

template<class... Args>
std::ostream& warning(const ASTNode* n, const char* fmt, Args... args) { return warning(n->location(), fmt, args...); }
template<class... Args>
std::ostream& error  (const ASTNode* n, const char* fmt, Args... args) { return error  (n->location(), fmt, args...); }

//------------------------------------------------------------------------------

class Identifier : public ASTNode {
public:
    Identifier(Location location, const char* str)
        : ASTNode(location)
        , symbol_(str)
    {}

    Identifier(Token tok)
        : ASTNode(tok.location())
        , symbol_(tok.symbol())
    {}

    Symbol symbol() const { return symbol_; }
    std::ostream& stream(std::ostream&) const override;

private:
    Symbol symbol_;
};

/*
 * paths
 */

class Path : public ASTNode {
public:
    class Elem : public ASTNode {
    public:
        Elem(const Identifier* id)
            : ASTNode(id->location())
            , identifier_(id)
        {}

        const Identifier* identifier() const { return identifier_.get(); }
        Symbol symbol() const { return identifier()->symbol(); }
        const Decl* decl() const { return decl_; }
        void check(NameSema&) const;
        std::ostream& stream(std::ostream&) const override;

    private:
        std::unique_ptr<const Identifier> identifier_;
        mutable const Decl* decl_ = nullptr;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    Path(Location location, bool global, Elems&& elems)
        : ASTNode(location)
        , global_(global)
        , elems_(std::move(elems))
    {}

    Path(const Identifier* id)
        : Path(id->location(), false, Elems())
    {
        elems_.emplace_back(new Elem(id));
    }

    bool is_global() const { return global_; }
    const Elems& elems() const { return elems_; }
    const Decl* decl() const { return elems().back()->decl(); }
    void check(NameSema&) const;
    std::ostream& stream(std::ostream&) const override;

private:
    bool global_;
    Elems elems_;
};

//------------------------------------------------------------------------------

/*
 * AST types
 */

class ASTType : public ASTNode, public Typeable {
public:
    ASTType(Location location)
        : ASTNode(location)
    {}

    virtual void check(NameSema&) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(Location location)
        : ASTType(location)
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
};

class PrimASTType : public ASTType {
public:
    enum Tag {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    PrimASTType(Location location, Tag tag)
        : ASTType(location)
        , tag_(tag)
    {}

    Tag tag() const { return tag_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
};

class PtrASTType : public ASTType {
public:
    enum Tag { Borrowed, Mut, Owned };

    PtrASTType(Location location, Tag tag, int addr_space, const ASTType* referenced_ast_type)
        : ASTType(location)
        , tag_(tag)
        , addr_space_(addr_space)
        , referenced_ast_type_(referenced_ast_type)
    {}

    Tag tag() const { return tag_; }
    std::string prefix() const;
    const ASTType* referenced_ast_type() const { return referenced_ast_type_.get(); }
    int addr_space() const { return addr_space_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    int addr_space_;
    std::unique_ptr<const ASTType> referenced_ast_type_;
};

class ArrayASTType : public ASTType {
public:
    ArrayASTType(Location location, const ASTType* elem_ast_type)
        : ASTType(location)
        , elem_ast_type_(elem_ast_type)
    {}

    const ASTType* elem_ast_type() const { return elem_ast_type_.get(); }

protected:
    std::unique_ptr<const ASTType> elem_ast_type_;
};

class IndefiniteArrayASTType : public ArrayASTType {
public:
    IndefiniteArrayASTType(Location location, const ASTType* elem_ast_type)
        : ArrayASTType(location, elem_ast_type)
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    DefiniteArrayASTType(Location location, const ASTType* elem_ast_type, uint64_t dim)
        : ArrayASTType(location, elem_ast_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    uint64_t dim_;
};

class CompoundASTType : public ASTType {
public:
    CompoundASTType(Location location, ASTTypes&& ast_type_args)
        : ASTType(location)
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
    TupleASTType(Location location, ASTTypes&& ast_type_args)
        : CompoundASTType(location, std::move(ast_type_args))
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
};

class ASTTypeApp : public CompoundASTType {
public:
    ASTTypeApp(Location location, const Path* path, ASTTypes&& ast_type_args)
        : CompoundASTType(location, std::move(ast_type_args))
        , path_(path)
    {}

    ASTTypeApp(Location location, const Path* path)
        : ASTTypeApp(location, path, ASTTypes())
    {}

    const Path* path() const { return path_.get(); }
    const Identifier* identifier() const { return path()->elems().back()->identifier(); }
    Symbol symbol() const { return identifier()->symbol(); }
    const Decl* decl() const { return path()->decl(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Path> path_;
};

class FnASTType : public ASTTypeParamList, public CompoundASTType {
public:
    FnASTType(Location location, ASTTypeParams&& ast_type_params, ASTTypes&& ast_type_args)
        : ASTTypeParamList(std::move(ast_type_params))
        , CompoundASTType(location, std::move(ast_type_args))
    {}

    FnASTType(Location location, ASTTypes&& ast_type_args = ASTTypes())
        : ASTTypeParamList(ASTTypeParams())
        , CompoundASTType(location, std::move(ast_type_args))
    {}

    const FnASTType* ret_fn_ast_type() const;

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
};

class Typeof : public ASTType {
public:
    Typeof(Location location, const Expr* expr)
        : ASTType(location)
        , expr_(dock(expr_, expr))
    {}

    const Expr* expr() const { return expr_.get(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> expr_;
};

class SimdASTType : public ArrayASTType {
public:
    SimdASTType(Location location, const ASTType* elem_ast_type, uint64_t size)
        : ArrayASTType(location, elem_ast_type)
        , size_(size)
    {}

    uint64_t size() const { return size_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    uint64_t size_;
};

//------------------------------------------------------------------------------

/*
 * declarations
 */

/// Base class for all entities which have a @p symbol_.
class Decl : public Typeable, public ASTNode {
public:
    enum Tag {
        NoDecl,
        NamedDecl,     ///< Just has an @p Identifier.
        TypeableDecl,  ///< Must have a @p Type assigned.
        TypeDecl,      ///< Represents a type definition like @p StructDecl or @p Typedef.
        ValueDecl,     ///< Represents something which is a value at run time.
    };

    /// General constructor.
    Decl(Tag tag, Location location, bool mut, const Identifier* id, const ASTType* ast_type)
        : ASTNode(location)
        , tag_(tag)
        , identifier_(id)
        , ast_type_(ast_type)
        , mut_(mut)
        , written_(false)
        , done_(false)
    {}

    /// @p NoDecl.
    Decl(Location location)
        : Decl(NoDecl, location, false, nullptr, nullptr)
    {}

    /// @p TypeableDecl, @p TypeDecl or @p ValueDecl.
    Decl(Tag tag, Location location, const Identifier* id)
        : Decl(tag, location, false, id, nullptr)
    {}

    /// @p ValueDecl.
    Decl(Location location, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(ValueDecl, location, mut, id, ast_type)
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
    bool is_anonymous() const { assert(!is_no_decl()); return symbol() == Symbol() || symbol().str()[0] == '<'; }
    size_t depth() const { assert(!is_no_decl()); return depth_; }
    const Decl* shadows() const { assert(!is_no_decl()); return shadows_; }
    thorin::Debug debug() const { return {location(), symbol().str()}; }

    // ValueDecl
    const ASTType* ast_type() const { assert(is_value_decl()); return ast_type_.get(); } ///< Original \p ASTType.
    bool is_mut() const { assert(is_value_decl()); return mut_; }
    bool is_written() const { assert(is_value_decl()); return written_; }
    void write() const { assert(is_value_decl()); written_ = true; }
    virtual thorin::Value emit(CodeGen&, const thorin::Def*) const { THORIN_UNREACHABLE; }

private:
    Tag tag_;
    std::unique_ptr<const Identifier> identifier_;
    std::unique_ptr<const ASTType> ast_type_;

protected:
    mutable thorin::Value value_;
    mutable const Decl* shadows_;
    mutable unsigned depth_   : 24;
    unsigned mut_             :  1;
    mutable unsigned written_ :  1;
    mutable unsigned done_    :  1; ///< Used during @p CodeGen.

    friend class CodeGen;
    friend class NameSema;
};

/// Base class for all values which may be mutated within a function.
class LocalDecl : public Decl {
public:
    LocalDecl(Location location, size_t handle, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(location, mut, id, ast_type)
        , handle_(handle)
    {}

    LocalDecl(Location location, size_t handle, const Identifier* id, const ASTType* ast_type)
        : LocalDecl(location, handle, /*mut*/ false, id, ast_type)
    {}

    size_t handle() const { return handle_; }
    bool is_address_taken() const { return is_address_taken_; }
    const Fn* fn() const { return fn_; }
    void take_address() const { is_address_taken_ = true; }
    void check(NameSema&) const;

    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const;
    void check(TypeSema&) const;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

protected:
    size_t handle_;
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
    ASTTypeParam(Location location, const Identifier* id, ASTTypes&& bounds)
        : Decl(TypeDecl, location, id)
        , bounds_(std::move(bounds))
    {}

    size_t num_bounds() const { return bounds().size(); }
    const ASTTypes& bounds() const { return bounds_; }
    int lambda_depth() const { return lambda_depth_; }
    const Var* var() const { return type()->as<Var>(); }

    void check(NameSema&) const;
    const Var* check(TypeSema&) const;
    std::ostream& stream(std::ostream&) const override;

private:
    const Var* check(InferSema&) const;

    ASTTypes bounds_;
    mutable int lambda_depth_ = -1;

    friend class ASTTypeApp;
    friend class ASTTypeParamList;
    friend class InferSema;
};

class Param : public LocalDecl {
public:
    Param(Location location, size_t handle, bool mut, const Identifier* id, const ASTType* ast_type)
        : LocalDecl(location, handle, mut, id, ast_type)
    {}

    Param(Location location, size_t handle, const Identifier* id, const ASTType* ast_type)
        : LocalDecl(location, handle, /*mut*/ false, id, ast_type)
    {}
};

class Fn : public ASTTypeParamList {
public:
    Fn(ASTTypeParams&& ast_type_params, Params&& params, const Expr* body)
        : ASTTypeParamList(std::move(ast_type_params))
        , params_(std::move(params))
        , body_(dock(body_, body))
    {}

    const Param* param(size_t i) const { return params_[i].get(); }
    ArrayRef<std::unique_ptr<const Param>> params() const { return params_; }
    size_t num_params() const { return params_.size(); }
    const Expr* body() const { return body_.get(); }
    thorin::Continuation* continuation() const { return continuation_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Def* frame() const { return frame_; }
    std::ostream& stream_params(std::ostream& p, bool returning) const;
    void fn_check(NameSema&) const;
    const Type* check_body(TypeSema&) const;
    thorin::Continuation* emit_head(CodeGen&, Location) const;
    void emit_body(CodeGen&, Location loc) const;

    virtual const FnType* fn_type() const = 0;
    virtual Symbol fn_symbol() const = 0;

protected:
    Params params_;
    mutable thorin::Continuation* continuation_ = nullptr;
    mutable const thorin::Param* ret_param_ = nullptr;
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
    Item(Location location, Visibility vis)
        : Decl(location)
        , visibility_(vis)
    {}

    /// @p TypeableDecl, @p TypeDecl or @p ValueDecl.
    Item(Tag tag, Location location, Visibility vis, const Identifier* id)
        : Decl(tag, location, id)
        , visibility_(vis)
    {}

    /// @p ValueDecl.
    Item(Location location, Visibility vis, bool mut, const Identifier* id, const ASTType* ast_type)
        : Decl(ValueDecl, location, mut, id, ast_type)
        , visibility_(vis)
    {}

    Visibility visibility() const { return visibility_; }
    virtual void check(NameSema&) const = 0;

private:
    virtual void check(InferSema&) const = 0;
    virtual const Type* check_head(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;

    Visibility visibility_;

    friend class CodeGen;
    friend class InferSema;
    friend class TypeSema;
};

class TypeDeclItem : public Item, public ASTTypeParamList {
public:
    TypeDeclItem(Location location, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params)
        : Item(TypeDecl, location,  vis, id)
        , ASTTypeParamList(std::move(ast_type_params))
    {}
};

class ValueItem : public Item {
public:
    ValueItem(Location location, Visibility vis, bool mut, const Identifier* id, const ASTType* ast_type)
        : Item(location, vis, mut, id, ast_type)
    {}

    void emit(CodeGen&) const override;
};

class Module : public TypeDeclItem {
public:
    Module(Location location, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params, Items&& items)
        : TypeDeclItem(location, vis, id, std::move(ast_type_params))
        , items_(std::move(items))
    {}

    Module(const char* first_file_name, Items&& items = Items())
        : Module(items.empty() ? Location(first_file_name, 1, 1) : Location(items.front()->location(), items.back()->location()),
                 Visibility::Pub, nullptr, ASTTypeParams(), std::move(items))
    {}

    const Items& items() const { return items_; }
    const Symbol2Item& symbol2item() const { return symbol2item_; }

    void check(NameSema&) const override;
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    Items items_;
    mutable Symbol2Item symbol2item_;
};

class ModuleDecl : public TypeDeclItem {
public:
    ModuleDecl(Location location, Visibility vis, const Identifier* id, ASTTypeParams&& ast_type_params)
        : TypeDeclItem(location, vis, id, std::move(ast_type_params))
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;
};

class ExternBlock : public Item {
public:
    ExternBlock(Location location, Visibility vis, Symbol abi, FnDecls&& fn_decls)
        : Item(location, vis)
        , abi_(abi)
        , fn_decls_(std::move(fn_decls))
    {}

    Symbol abi() const { return abi_; }
    const FnDecls& fn_decls() const { return fn_decls_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

    Symbol abi_;
    FnDecls fn_decls_;
};

class Typedef : public TypeDeclItem {
public:
    Typedef(Location location, Visibility vis, const Identifier* id,
            ASTTypeParams&& ast_type_params, const ASTType* ast_type)
        : TypeDeclItem(location, vis, id, std::move(ast_type_params))
        , ast_type_(ast_type)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

    std::unique_ptr<const ASTType> ast_type_;
};

class FieldDecl : public Decl {
public:
    FieldDecl(Location location, size_t index, Visibility vis, const Identifier* id, const ASTType* ast_type)
        : Decl(TypeableDecl, location, id)
        , index_(index)
        , visibility_(vis)
        , ast_type_(std::move(ast_type))
    {}

    uint32_t index() const { return index_; }
    const ASTType* ast_type() const { return ast_type_.get(); }
    Visibility visibility() const { return  visibility_; }

    void check(NameSema&) const;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const;
    void check(TypeSema&) const;

    uint32_t index_;
    Visibility visibility_;
    std::unique_ptr<const ASTType> ast_type_;

    friend class InferSema;
    friend class TypeSema;
};

class StructDecl : public TypeDeclItem {
public:
    StructDecl(Location location, Visibility vis, const Identifier* id,
               ASTTypeParams&& ast_type_params, FieldDecls&& field_decls)
        : TypeDeclItem(location, vis, id, std::move(ast_type_params))
        , field_decls_(std::move(field_decls))
    {}

    size_t num_field_decls() const { return field_decls_.size(); }
    const FieldDecls& field_decls() const { return field_decls_; }
    const FieldTable& field_table() const { return field_table_; }
    const FieldDecl* field_decl(size_t i) const { return field_decls_[i].get(); }
    const FieldDecl* field_decl(Symbol symbol) const { return thorin::find(field_table_, symbol); }
    const FieldDecl* field_decl(const Identifier* ident) const { return field_decl(ident->symbol()); }
    const StructType* struct_type() const { return type_->as<StructType>(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

    FieldDecls field_decls_;
    mutable FieldTable field_table_;
};

class EnumDecl : public TypeDeclItem {
public:
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream& os) const override { return os; }

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override {}
};

class StaticItem : public ValueItem {
public:
    StaticItem(Location location, Visibility vis, bool mut, const Identifier* id,
               const ASTType* ast_type, const Expr* init)
        : ValueItem(location, vis, mut, id, std::move(ast_type))
        , init_(dock(init_, init))
    {}

    const Expr* init() const { return init_.get(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    std::unique_ptr<const Expr> init_;
};

class FnDecl : public ValueItem, public Fn {
public:
    FnDecl(Location location, Visibility vis, bool is_extern, Symbol abi, Symbol export_name,
           const Identifier* id, ASTTypeParams&& ast_type_params, Params&& params, const Expr* body)
        : ValueItem(location, vis, /*mut*/ false, id, /*ast_type*/ nullptr)
        , Fn(std::move(ast_type_params), std::move(params), body)
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
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    Symbol abi_;
    Symbol export_name_;
    bool is_extern_ = false;
};

class TraitDecl : public Item, public ASTTypeParamList {
public:
    TraitDecl(Location location, Visibility vis, const Identifier* id,
              ASTTypeParams&& ast_type_params, ASTTypeApps&& super_traits, FnDecls&& methods)
        : Item(TypeDecl, location, vis, id)
        , ASTTypeParamList(std::move(ast_type_params))
        , super_traits_(std::move(super_traits))
        , methods_(std::move(methods))
    {}

    const ASTTypeApps& super_traits() const { return super_traits_; }
    const FnDecls& methods() const { return methods_; }
    const MethodTable& method_table() const { return method_table_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

    ASTTypeApps super_traits_;
    FnDecls methods_;
    mutable MethodTable method_table_;
};

class ImplItem : public Item, public ASTTypeParamList {
public:
    ImplItem(Location location, Visibility vis, ASTTypeParams&& ast_type_params,
             const ASTType* trait, const ASTType* ast_type, FnDecls&& methods)
        : Item(location, vis)
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

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    const Type* check_head(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

    std::unique_ptr<const ASTType> trait_;
    std::unique_ptr<const ASTType> ast_type_;
    FnDecls methods_;
    mutable const thorin::Def* def_;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode, public Typeable {
public:
    Expr(Location location)
        : ASTNode(location)
    {}

#ifndef NDEBUG
    virtual ~Expr() { assert(back_ref_ != nullptr); }
#endif

    const thorin::Def* extra() const { return extra_; }

    virtual void write() const {}
    virtual bool has_side_effect() const { return false; }
    virtual void take_address() const {}
    virtual void check(NameSema&) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual thorin::Value lemit(CodeGen&) const;
    virtual const thorin::Def* remit(CodeGen&) const;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const;

protected:
    /// Needed to propagate extend of indefinite arrays.
    mutable const thorin::Def* extra_ = nullptr;

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
    std::unique_ptr<const Expr> ptr;
    auto parent = expr->back_ref_;
    parent->release();
    expr->back_ref_ = nullptr;
    auto new_expr = new T(std::forward<Args>(args)...);
    parent->reset(new_expr);
    new_expr->back_ref_ = parent;
    return new_expr;
}

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
    std::ostream& stream_args(std::ostream& p) const;

protected:
    Exprs args_;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(Location location)
        : Expr(location)
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
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

    LiteralExpr(Location location, Tag tag, thorin::Box box)
        : Expr(location)
        , tag_(tag)
        , box_(box)
    {}

    Tag tag() const { return tag_; }
    thorin::Box box() const { return box_; }
    uint64_t get_u64() const;
    PrimTypeTag literal2type() const;

    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    thorin::Box box_;
};

class CharExpr : public Expr {
public:
    CharExpr(Location location, Symbol symbol, char value)
        : Expr(location)
        , symbol_(symbol)
        , value_(value)
    {}

    Symbol symbol() const { return symbol_; }
    char value() const { return value_; }

    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbol symbol_;
    char value_;
};

class StrExpr : public Expr {
public:
    StrExpr(Location location, Symbols&& symbols, std::vector<char>&& values)
        : Expr(location)
        , symbols_(std::move(symbols))
        , values_(std::move(values))
    {}

    const Symbols& symbols() const { return symbols_; }
    const std::vector<char>& values() const { return values_; }

    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Symbols symbols_;
    mutable std::vector<char> values_;
};

class FnExpr : public Expr, public Fn {
public:
    FnExpr(Location location, Params&& params, const Expr* body)
        : Expr(location)
        , Fn(ASTTypeParams(), std::move(params), body)
    {}

    const FnType* fn_type() const override { return type()->as<FnType>(); }
    Symbol fn_symbol() const override { return Symbol("lambda"); }
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    size_t ret_var_handle_;
};

class PathExpr : public Expr {
public:
    PathExpr(const Path* path)
        : Expr(path->location())
        , path_(path)
    {}

    const Path* path() const { return path_.get(); }
    const Decl* value_decl() const { return value_decl_; }

    void write() const override;
    void take_address() const override;
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;

    std::unique_ptr<const Path> path_;
    mutable const Decl* value_decl_ = nullptr; ///< Declaration of the variable in use.
};

class PrefixExpr : public Expr {
public:
    enum Tag {
#define IMPALA_PREFIX(tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
        MUT
    };

    PrefixExpr(Location location, Tag tag, const Expr* rhs)
        : Expr(location)
        , tag_(tag)
        , rhs_(dock(rhs_, rhs))
    {}

    static const PrefixExpr* create(const Expr* rhs, const Tag tag) {
        return interlope<PrefixExpr>(rhs, rhs->location(), tag, rhs);
    }

    static const PrefixExpr* create_deref(const Expr* rhs) { return create(rhs, MUL); }
    static const PrefixExpr* create_addrof(const Expr* rhs) { return create(rhs, AND); }

    const Expr* rhs() const { return rhs_.get(); }
    Tag tag() const { return tag_; }

    void write() const override;
    bool has_side_effect() const override;
    void check(NameSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    std::unique_ptr<const Expr> rhs_;
};

class InfixExpr : public Expr {
public:
    enum Tag {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) tok = Token:: tok,
#define IMPALA_INFIX(     tok, str, lprec, rprec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    InfixExpr(Location location, const Expr* lhs, Tag tag, const Expr* rhs)
        : Expr(location)
        , tag_(tag)
        , lhs_(dock(lhs_, lhs))
        , rhs_(dock(rhs_, rhs))
    {}

    Tag tag() const { return tag_; }
    const Expr* lhs() const { return lhs_.get(); }
    const Expr* rhs() const { return rhs_.get(); }

    bool has_side_effect() const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
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

    PostfixExpr(Location location, const Expr* lhs, Tag tag)
        : Expr(location)
        , tag_(tag)
        , lhs_(dock(lhs_, lhs))
    {}

    Tag tag() const { return tag_; }
    const Expr* lhs() const { return lhs_.get(); }

    bool has_side_effect() const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Tag tag_;
    std::unique_ptr<const Expr> lhs_;
};

class FieldExpr : public Expr {
public:
    FieldExpr(Location location, const Expr* lhs, const Identifier* id)
        : Expr(location)
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
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;
    std::unique_ptr<const Identifier> identifier_;
    mutable const FieldDecl* field_decl_ = nullptr;
};

class CastExpr : public Expr {
public:
    CastExpr(Location location, const Expr* src)
        : Expr(location)
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
    ExplicitCastExpr(Location location, const Expr* src, const ASTType* ast_type)
        : CastExpr(location, src)
        , ast_type_(ast_type)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const ASTType> ast_type_;
};

class ImplicitCastExpr : public CastExpr {
public:
    ImplicitCastExpr(const Expr* src, const Type* type)
        : CastExpr(src->location(), src)
    {
        type_ = type;
    }

    static const ImplicitCastExpr* create(const Expr* src, const Type* type) {
        return interlope<ImplicitCastExpr>(src, src, type);
    }

    void check(NameSema&) const override { THORIN_UNREACHABLE; }
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
};

class Ref2ValueExpr : public CastExpr {
public:
    Ref2ValueExpr(const Expr* src)
        : CastExpr(src->location(), src)
    {
        type_ = src->type()->as<RefType>()->pointee();
    }

    static const Ref2ValueExpr* create(const Expr* src) {
        return interlope<Ref2ValueExpr>(src, src);
    }

    void check(NameSema&) const override { THORIN_UNREACHABLE; }
    std::ostream& stream(std::ostream&) const override;

private:
    void check(TypeSema&) const override;
    const Type* check(InferSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class DefiniteArrayExpr : public Expr, public Args {
public:
    DefiniteArrayExpr(Location location, Exprs&& args)
        : Expr(location)
        , Args(std::move(args))
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    RepeatedDefiniteArrayExpr(Location location, const Expr* value, uint64_t count)
        : Expr(location)
        , value_(dock(value_, value))
        , count_(count)
    {}

    const Expr* value() const { return value_.get(); }
    uint64_t count() const { return count_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> value_;
    uint64_t count_;
};

class IndefiniteArrayExpr : public Expr {
public:
    IndefiniteArrayExpr(Location location, const Expr* dim, const ASTType* elem_ast_type)
        : Expr(location)
        , dim_(dock(dim_, dim))
        , elem_ast_type_(elem_ast_type)
    {}

    const Expr* dim() const { return dim_.get(); }
    const ASTType* elem_ast_type() const { return elem_ast_type_.get(); }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> dim_;
    std::unique_ptr<const ASTType> elem_ast_type_;
};

class TupleExpr : public Expr, public Args {
public:
    TupleExpr(Location location, Exprs&& args)
        : Expr(location)
        , Args(std::move(args))
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class SimdExpr : public Expr, public Args {
public:
    SimdExpr(Location location, Exprs&& args)
        : Expr(location)
        , Args(std::move(args))
    {}

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class StructExpr : public Expr {
public:
    class Elem : public ASTNode {
    public:
        Elem(Location location, const Identifier* id, const Expr* expr)
            : ASTNode(location)
            , identifier_(id)
            , expr_(dock(expr_, expr))
        {}

        const Identifier* identifier() const { return identifier_.get(); }
        Symbol symbol() const { return identifier()->symbol(); }
        const Expr* expr() const { return expr_.get(); }
        const FieldDecl* field_decl() const { return field_decl_; }

        std::ostream& stream(std::ostream&) const override;

    private:
        std::unique_ptr<const Identifier> identifier_;
        std::unique_ptr<const Expr> expr_;
        mutable const FieldDecl* field_decl_ = nullptr;

        friend class StructExpr;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    StructExpr(Location location, const ASTTypeApp* ast_type_app, Elems&& elems)
        : Expr(location)
        , ast_type_app_(ast_type_app)
        , elems_(std::move(elems))
    {}

    const ASTTypeApp* ast_type_app() const { return ast_type_app_.get(); }
    const Elem* elem(size_t i) const { return elems_[i].get(); }
    size_t num_elems() const { return elems_.size(); }
    const Elems& elems() const { return elems_; }

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const ASTTypeApp> ast_type_app_;
    Elems elems_;
};

class TypeAppExpr : public Expr {
public:
    TypeAppExpr(Location location, const Expr* lhs, ASTTypes&& ast_type_args)
        : Expr(location)
        , lhs_(dock(lhs_, lhs))
        , ast_type_args_(std::move(ast_type_args))
    {}

    static const TypeAppExpr* create(const Expr* lhs) {
        return interlope<TypeAppExpr>(lhs, lhs->location(), lhs, ASTTypes());
    }

    const Expr* lhs() const { return lhs_.get(); }
    const ASTTypes& ast_type_args() const { return ast_type_args_; }
    const ASTType* ast_type_arg(size_t i) const { assert(i < ast_type_args_.size()); return ast_type_args_[i].get(); }
    size_t num_ast_type_args() const { return ast_type_args().size(); }
    Types type_args() const { return type_args_; }
    const Type*& type_arg(size_t i) const { return type_args_[i]; }
    size_t num_type_args() const { return type_args_.size(); }
    std::ostream& stream_ast_type_args(std::ostream& p) const;
    std::ostream& stream_type_args(std::ostream& p) const;

    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;
    ASTTypes ast_type_args_;
    mutable std::vector<const Type*> type_args_;
};

class MapExpr : public Expr, public Args {
public:
    MapExpr(Location location, const Expr* lhs, Exprs&& args)
        : Expr(location)
        , Args(std::move(args))
        , lhs_(dock(lhs_, lhs))
    {}

    enum State {
        None, Run, Hlt
    };

    const Expr* lhs() const { return lhs_.get(); }

    void write() const override;
    bool has_side_effect() const override;
    void take_address() const override;
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&, State, Location) const;

    std::unique_ptr<const Expr> lhs_;

    friend class CodeGen;
};

class StmtLikeExpr : public Expr {
protected:
    StmtLikeExpr(Location location)
        : Expr(location)
    {}
};

class BlockExprBase : public StmtLikeExpr {
public:
    BlockExprBase(Location location, Stmts&& stmts, const Expr* expr)
        : StmtLikeExpr(location)
        , stmts_(std::move(stmts))
        , expr_(dock(expr_, expr))
    {}

    const Stmts& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_.get(); }
    const Stmt* stmt(size_t i) const { return stmts_[i].get(); }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    const LocalDecls& locals() const { return locals_; }
    void add_local(const LocalDecl* local) const { locals_.push_back(local); }

    virtual const char* prefix() const = 0;
    bool has_side_effect() const override;
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

protected:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    Stmts stmts_;
    std::unique_ptr<const Expr> expr_;
    mutable LocalDecls locals_; ///< All \p LocalDecl%s in this \p BlockExprBase from top to bottom.
};

class BlockExpr : public BlockExprBase {
public:
    BlockExpr(Location location, Stmts&& stmts, const Expr* expr)
        : BlockExprBase(location, std::move(stmts), expr)
    {}

    BlockExpr(Location location)
        : BlockExprBase(location, Stmts(), new EmptyExpr(location))
    {}

    const char* prefix() const override { return "{"; }
};

class RunBlockExpr : public BlockExprBase {
public:
    RunBlockExpr(Location location, Stmts&& stmts, const Expr* expr)
        : BlockExprBase(location, std::move(stmts), expr)
    {}

    const char* prefix() const override { return "@{"; }

private:
    const thorin::Def* remit(CodeGen&) const override;
};

class IfExpr : public StmtLikeExpr {
public:
    IfExpr(Location location, const Expr* cond, const Expr* then_expr, const Expr* else_expr)
        : StmtLikeExpr(location)
        , cond_(dock(cond_, cond))
        , then_expr_(dock(then_expr_, then_expr))
        , else_expr_(dock(else_expr_, else_expr))
    {}

    const Expr* cond() const { return cond_.get(); }
    const Expr* then_expr() const { return then_expr_.get(); }
    const Expr* else_expr() const { return else_expr_.get(); }
    bool has_else() const;

    bool has_side_effect() const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_jump(CodeGen&, thorin::JumpTarget&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> cond_;
    std::unique_ptr<const Expr> then_expr_;
    std::unique_ptr<const Expr> else_expr_;
};

class WhileExpr : public StmtLikeExpr {
public:
    WhileExpr(Location location, const LocalDecl* continue_decl, const Expr* cond,
              const Expr* body, const LocalDecl* break_decl)
        : StmtLikeExpr(location)
        , continue_decl_(continue_decl)
        , cond_(dock(cond_, cond))
        , body_(dock(body_, body))
        , break_decl_(break_decl)
    {}

    const Expr* cond() const { return cond_.get(); }
    const BlockExprBase* body() const { return body_.get()->as<BlockExprBase>(); }
    const LocalDecl* break_decl() const { return break_decl_.get(); }
    const LocalDecl* continue_decl() const { return continue_decl_.get(); }

    bool has_side_effect() const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_jump(CodeGen&, thorin::JumpTarget&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const LocalDecl> continue_decl_;
    std::unique_ptr<const Expr> cond_;
    std::unique_ptr<const Expr> body_;
    std::unique_ptr<const LocalDecl> break_decl_;
};

class ForExpr : public StmtLikeExpr {
public:
    ForExpr(Location location, const Expr* fn_expr, const Expr* expr, const LocalDecl* break_decl)
        : StmtLikeExpr(location)
        , fn_expr_(dock(fn_expr_, fn_expr))
        , expr_(dock(expr_, expr))
        , break_decl_(break_decl)
    {}

    const FnExpr* fn_expr() const { return fn_expr_.get()->as<FnExpr>(); }
    const Expr* expr() const { return expr_.get(); }
    const LocalDecl* break_decl() const { return break_decl_.get(); }

    bool has_side_effect() const override;
    void check(NameSema&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
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

class Ptrn : public ASTNode, public Typeable {
public:
    Ptrn(Location location)
        : ASTNode(location)
    {}

    virtual void check(NameSema&) const = 0;
    virtual void emit(CodeGen&, const thorin::Def*) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class TuplePtrn : public Ptrn {
public:
    TuplePtrn(Location location, Ptrns&& elems)
        : Ptrn(location)
        , elems_(std::move(elems))
    {}

    const Ptrns& elems() const { return elems_; }
    const Ptrn* elem(size_t i) const { return elems_[i].get(); }
    size_t num_elems() const { return elems_.size(); }

    void check(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Ptrns elems_;
};

class IdPtrn : public Ptrn {
public:
    IdPtrn(const LocalDecl* local)
        : Ptrn(local->location())
        , local_(local)
    {}

    const LocalDecl* local() const { return local_.get(); }

    void check(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const LocalDecl> local_;
};

//------------------------------------------------------------------------------

/*
 * statements
 */

class Stmt : public ASTNode {
public:
    Stmt(Location location)
        : ASTNode(location)
    {}

    virtual void check(NameSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;

private:
    virtual void check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class ExprStmt : public Stmt {
public:
    ExprStmt(Location location, const Expr* expr)
        : Stmt(location)
        , expr_(dock(expr_, expr))
    {}

    const Expr* expr() const { return expr_.get(); }

    void check(NameSema&) const override;
    void emit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Expr> expr_;
};

class ItemStmt : public Stmt {
public:
    ItemStmt(Location location, const Item* item)
        : Stmt(location)
        , item_(item)
    {}

    const Item* item() const { return item_.get(); }

    void check(NameSema&) const override;
    void emit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Item> item_;
};

class LetStmt : public Stmt {
public:
    LetStmt(Location location, const Ptrn* ptrn, const Expr* init)
        : Stmt(location)
        , ptrn_(ptrn)
        , init_(dock(init_, init))
    {}

    const Ptrn* ptrn() const { return ptrn_.get(); }
    const Expr* init() const { return init_.get(); }

    void check(NameSema&) const override;
    void emit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Ptrn> ptrn_;
    std::unique_ptr<const Expr> init_;
};

class AsmStmt : public Stmt {
public:
    class Elem : public ASTNode {
    public:
        Elem(Location location, std::string&& constraint, const Expr* expr)
            : ASTNode(location)
            , constraint_(std::move(constraint))
            , expr_(dock(expr_, expr))
        {}

        const std::string& constraint() const { return constraint_; }
        const Expr* expr() const { return expr_.get(); }

        std::ostream& stream(std::ostream&) const override;

    private:
        std::string constraint_;
        std::unique_ptr<const Expr> expr_;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    AsmStmt(Location location, std::string&& asm_template, Elems&& outputs, Elems&& inputs,
            Strings&& clobbers, Strings&& options)
        : Stmt(location)
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

    void check(NameSema&) const override;
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

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
