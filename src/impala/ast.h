#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/util/array.h"
#include "thorin/util/assert.h"
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
class MapExpr;
class NamedItem;
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
typedef thorin::HashMap<Symbol, const NamedItem*> ItemTable;

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

    friend class ForExpr;
    friend class InferSema;
    friend class TypeSema;
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

template<typename... Args>
std::ostream& warning(const ASTNode* n, const char* fmt, Args... args) { return warning(n->location(), fmt, args...); }
template<typename... Args>
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
    virtual std::ostream& stream(std::ostream&) const;

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
        Elem(const Identifier* identifier)
            : ASTNode(identifier->location())
            , identifier_(identifier)
        {}

        const Identifier* identifier() const { return identifier_.get(); }
        Symbol symbol() const { return identifier()->symbol(); }
        const Decl* decl() const { return decl_; }
        void check(NameSema&) const;
        std::ostream& stream(std::ostream&) const override;

    private:
        std::unique_ptr<const Identifier> identifier_;
        mutable const Decl* decl_ = nullptr;

        friend class Path;
    };

    typedef std::deque<std::unique_ptr<const Elem>> Elems;

    Path(Location location, bool global, Elems&& elems)
        : ASTNode(location)
        , global_(global)
        , elems_(std::move(elems))
    {}

    Path(const Identifier* identifier)
        : Path(identifier->location(), false, Elems())
    {
        elems_.emplace_back(new Elem(identifier));
    }

    //// HACK
    //Path(Location location, bool global, const Identifier* identifier)
        //: ASTNode(location)
        //, global_(global)
    //{
        //elems_.emplace_back(std::make_unique<Elem>(identifier));
    //}

    bool is_global() const { return global_; }
    const Elems& elems() const { return elems_; }
    const Decl* decl() const { return elems().back()->decl(); }
    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const;

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

    friend class NameSema;
    friend class InferSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(Location location)
        : ASTType(location)
    {}

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
};

class PrimASTType : public ASTType {
public:
    enum Kind {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    PrimASTType(Location location, Kind kind)
        : ASTType(location)
        , kind_(kind)
    {}

    Kind kind() const { return kind_; }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
};

class PtrASTType : public ASTType {
public:
    enum Kind { Borrowed, Mut, Owned };

    PtrASTType(Location location, Kind kind, int addr_space, const ASTType* referenced_ast_type)
        : ASTType(location)
        , kind_(kind)
        , addr_space_(addr_space)
        , referenced_ast_type_(referenced_ast_type)
    {}

    Kind kind() const { return kind_; }
    std::string prefix() const;
    const ASTType* referenced_ast_type() const { return referenced_ast_type_.get(); }
    int addr_space() const { return addr_space_; }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    std::unique_ptr<const Path> path_;

    friend class NameScope;
};

class FnASTType : public ASTTypeParamList, public CompoundASTType {
public:
    FnASTType(Location location, ASTTypeParams&& ast_type_params, ASTTypes&& ast_type_args)
        : ASTTypeParamList(std::move(ast_type_params))
        , CompoundASTType(location, std::move(ast_type_args))
    {}

    const FnASTType* ret_fn_ast_type() const;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    uint64_t size_;
};

//------------------------------------------------------------------------------

/*
 * declarations
 */

/// Base class for all entities which have a \p symbol_.
class Decl : public ASTNode {
public:
    Decl(Location location, const Identifier* identifier)
        : ASTNode(location)
        , identifier_(identifier)
    {}

    const Identifier* identifier() const { return identifier_.get(); }
    Symbol symbol() const { return identifier_->symbol(); }
    size_t depth() const { return depth_; }
    const Decl* shadows() const { return shadows_; }

protected:
    std::unique_ptr<const Identifier> identifier_;

private:
    mutable const Decl* shadows_;
    mutable size_t depth_;

    friend class NameSema;
};

/// Base class for all declarations which must have a \p Type assigned.
class TypeableDecl : public Decl, public Typeable {
protected:
    TypeableDecl(Location location, const Identifier* identifier)
        : Decl(location, identifier)
    {}
};

/// Base class for all declarations which represent a type definition.
class TypeDecl : public TypeableDecl {
protected:
    TypeDecl(Location location, const Identifier* identifier)
        : TypeableDecl(location, identifier)
    {}
};

/// Base class for all declarations which represent a value.
class ValueDecl : public TypeableDecl {
public:
    ValueDecl(Location location, bool mut, const Identifier* identifier, const ASTType* ast_type)
        : TypeableDecl(location, identifier)
        , ast_type_(ast_type)
        , mut_(mut)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); } ///< Original \p ASTType.
    bool is_mut() const { return mut_; }
    bool is_written() const { return is_written_; }
    void write() const { is_written_ = true; }
    bool is_anonymous() const { return symbol() == Symbol() || symbol().str()[0] == '<'; }
    std::ostream& stream(std::ostream&) const override;

private:
    virtual thorin::Value emit(CodeGen&, const thorin::Def* init) const = 0;

protected:
    std::unique_ptr<const ASTType> ast_type_;
    mutable thorin::Value value_;
    bool mut_;
    mutable bool is_written_ = false;

    friend class CodeGen;
};

/// Base class for all values which may be mutated within a function.
class LocalDecl : public ValueDecl {
public:
    LocalDecl(Location location, size_t handle, bool mut, const Identifier* identifier, const ASTType* ast_type)
        : ValueDecl(location, mut, identifier, ast_type)
        , handle_(handle)
    {}

    LocalDecl(Location location, size_t handle, const Identifier* identifier, const ASTType* ast_type)
        : LocalDecl(location, handle, /*mut*/ false, identifier, ast_type)
    {}

    size_t handle() const { return handle_; }
    bool is_address_taken() const { return is_address_taken_; }
    const Fn* fn() const { return fn_; }
    void take_address() const { is_address_taken_ = true; }
    void check(NameSema&) const;

private:
    const Type* check(InferSema&) const;
    void check(TypeSema&) const;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

protected:
    size_t handle_;
    mutable const Fn* fn_;
    mutable bool is_address_taken_ = false;

    friend class InferSema;
    friend class TypeSema;
    friend class ValueDecl;
};

//------------------------------------------------------------------------------

/*
 * parameters and Fn
 */

class ASTTypeParam : public TypeDecl {
public:
    ASTTypeParam(Location location, const Identifier* identifier, ASTTypes&& bounds)
        : TypeDecl(location, identifier)
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

public: // HACK
    mutable int lambda_depth_ = -1;

    friend class InferSema;
    friend class TypeSema;
};

class Param : public LocalDecl {
public:
    Param(Location location, size_t handle, bool mut, const Identifier* identifier, const ASTType* ast_type)
        : LocalDecl(location, handle, mut, identifier, ast_type)
    {}

    Param(Location location, size_t handle, const Identifier* identifier, const ASTType* ast_type)
        : LocalDecl(location, handle, /*mut*/ false, identifier, ast_type)
    {}

    friend class Fn;
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
    thorin::Continuation* emit_head(CodeGen&, const Location&) const;
    void emit_body(CodeGen&, const Location& loc) const;

    bool is_continuation() const { return is_continuation_; }

    virtual const FnType* fn_type() const = 0;
    virtual Symbol fn_symbol() const = 0;

protected:
    Params params_;
    mutable thorin::Continuation* continuation_;
    mutable const thorin::Param* ret_param_ = nullptr;
    mutable const thorin::Def* frame_ = nullptr;

private:
    std::unique_ptr<const Expr> body_;
    bool is_continuation_; //< TODO remove this!!!

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * items
 */

class Item : public thorin::MagicCast<Item> {
public:
    Item(Visibility visibility)
        : visibility_(visibility)
    {}

    virtual ~Item() {}

    Visibility visibility() const { return  visibility_; }
    virtual void check(NameSema&) const = 0;

private:
    virtual void check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual void emit_item(CodeGen&) const = 0;

    Visibility visibility_;
#ifndef NDEBUG
    mutable bool done_ = false;
#endif

    friend class NameSema;
    friend class InferSema;
    friend class TypeSema;
    friend class CodeGen;
};

class NamedItem : public Item {
public:
    NamedItem(Visibility visibility)
        : Item(visibility)
    {}

    virtual const Identifier* item_identifier() const = 0;
    Symbol item_symbol() const { return item_identifier()->symbol(); }
};

class TypeDeclItem : public NamedItem, public TypeDecl, public ASTTypeParamList {
public:
    TypeDeclItem(Location location, Visibility visibility,
                 const Identifier* identifier, ASTTypeParams&& ast_type_params)
        : NamedItem(visibility)
        , TypeDecl(location, identifier)
        , ASTTypeParamList(std::move(ast_type_params))
    {}

    const Identifier* item_identifier() const override { return TypeDecl::identifier(); }
};

class ValueItem : public NamedItem, public ValueDecl {
public:
    ValueItem(Location location, Visibility visibility, bool mut,
              const Identifier* identifier, const ASTType* ast_type)
        : NamedItem(visibility)
        , ValueDecl(location, mut, identifier, std::move(ast_type))
    {}

    const Identifier* item_identifier() const override { return ValueDecl::identifier(); }

private:
    void emit_item(CodeGen&) const override;
};

class Module : public TypeDeclItem {
public:
    Module(Location location, Visibility visibility, const Identifier* identifier,
           ASTTypeParams&& ast_type_params, Items&& items)
        : TypeDeclItem(location, visibility, identifier, std::move(ast_type_params))
        , items_(std::move(items))
    {}

    Module(const char* first_file_name, Items&& items)
        : TypeDeclItem({first_file_name, 1, 1, 1, 1}, Visibility::Pub, nullptr, ASTTypeParams())
        , items_(std::move(items))
    {
        // TODO add location
        //if (!items.empty())
    }

    const Items& items() const { return items_; }
    const ItemTable& item_table() const { return item_table_; }

    void check(NameSema&) const override;
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;
    std::ostream& stream(std::ostream&) const override;

private:
    Items items_;
    mutable ItemTable item_table_;
};

class ModuleDecl : public TypeDeclItem {
public:
    ModuleDecl(Location location, Visibility vis, const Identifier* identifier, ASTTypeParams&& ast_type_params)
        : TypeDeclItem(location, vis, identifier, std::move(ast_type_params))
    {}

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;
};

class ExternBlock : public ASTNode, public Item {
public:
    ExternBlock(Location location, Visibility visibility, Symbol abi, FnDecls&& fn_decls)
        : ASTNode(location)
        , Item(visibility)
        , abi_(abi)
        , fn_decls_(std::move(fn_decls))
    {}

    Symbol abi() const { return abi_; }
    const FnDecls& fn_decls() const { return fn_decls_; }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;

    Symbol abi_;
    FnDecls fn_decls_;
};

class Typedef : public TypeDeclItem {
public:
    Typedef(Location location, Visibility visibility, const Identifier* identifier,
            ASTTypeParams&& ast_type_params, const ASTType* ast_type)
        : TypeDeclItem(location, visibility, identifier, std::move(ast_type_params))
        , ast_type_(ast_type)
    {}

    const ASTType* ast_type() const { return ast_type_.get(); }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;

    std::unique_ptr<const ASTType> ast_type_;
};

class FieldDecl : public TypeableDecl {
public:
    FieldDecl(Location location, size_t index, Visibility visibility,
              const Identifier* identifier, const ASTType* ast_type)
        : TypeableDecl(location, identifier)
        , index_(index)
        , visibility_(visibility)
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
    StructDecl(Location location, Visibility visibility, const Identifier* identifier,
               ASTTypeParams&& ast_type_params, FieldDecls&& field_decls)
        : TypeDeclItem(location, visibility, identifier, std::move(ast_type_params))
        , field_decls_(std::move(field_decls))
    {}

    size_t num_field_decls() const { return field_decls_.size(); }
    const FieldDecls& field_decls() const { return field_decls_; }
    const FieldTable& field_table() const { return field_table_; }
    const FieldDecl* field_decl(size_t i) const { return field_decls_[i].get(); }
    const FieldDecl* field_decl(Symbol symbol) const { return thorin::find(field_table_, symbol); }
    const FieldDecl* field_decl(const Identifier* ident) const { return field_decl(ident->symbol()); }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;

    FieldDecls field_decls_;
    mutable FieldTable field_table_;
};

class EnumDecl : public TypeDeclItem {
public:
    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;
};

class StaticItem : public ValueItem {
public:
    StaticItem(Location location, Visibility visibility, bool mut, const Identifier* identifier,
               const ASTType* ast_type, const Expr* init)
        : ValueItem(location, visibility, mut, identifier, std::move(ast_type))
        , init_(dock(init_, init))
    {}

    const Expr* init() const { return init_.get(); }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    std::unique_ptr<const Expr> init_;
};

class FnDecl : public ValueItem, public Fn {
public:
    bool is_extern() const { return is_extern_; }
    Symbol abi() const { return abi_; }

    const FnType* fn_type() const override {
        auto t = type();
        while (auto lambda = t->isa<Lambda>())
            t = lambda->body();
        return t->as<FnType>();
    }
    Symbol fn_symbol() const override { return export_name_ ? export_name_->symbol() : identifier()->symbol(); }
    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    std::unique_ptr<const Identifier> export_name_;
    bool is_extern_ = false;
    Symbol abi_;

    friend class InferSema;
    friend class TypeSema;
};

class TraitDecl : public NamedItem, public Decl, public ASTTypeParamList {
public:
    TraitDecl(Location location, Visibility visibility, const Identifier* identifier,
              ASTTypeParams&& ast_type_params, ASTTypeApps&& super_traits, FnDecls&& methods)
        : NamedItem(visibility)
        , Decl(location, identifier)
        , ASTTypeParamList(std::move(ast_type_params))
        , super_traits_(std::move(super_traits))
        , methods_(std::move(methods))
    {}

    const ASTTypeApps& super_traits() const { return super_traits_; }
    const FnDecls& methods() const { return methods_; }
    const MethodTable& method_table() const { return method_table_; }

    const Identifier* item_identifier() const override { return Decl::identifier(); }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;

    ASTTypeApps super_traits_;
    FnDecls methods_;
    mutable MethodTable method_table_;
};

class ImplItem : public ASTNode, public Item, public ASTTypeParamList {
public:
    ImplItem(Location location, Visibility visibility, ASTTypeParams&& ast_type_params,
             const ASTType* trait, const ASTType* ast_type, FnDecls&& methods)
        : ASTNode(location)
        , Item(visibility)
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
    void check(TypeSema&) const override;
    void emit_item(CodeGen&) const override;

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
    virtual ~Expr() { assert(docker_ != nullptr); }
#endif

    const thorin::Def* extra() const { return extra_; }

    virtual bool is_lvalue() const { return false; }
    virtual bool has_side_effect() const { return false; }
    virtual void take_address() const {}

    virtual std::ostream& stream(std::ostream&) const = 0;
    virtual void check(NameSema&) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual thorin::Value lemit(CodeGen&) const;
    virtual const thorin::Def* remit(CodeGen&) const;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const;

    mutable std::unique_ptr<const Expr>* docker_ = nullptr;

protected:
    mutable const thorin::Def* extra_ = nullptr; ///< Needed to propagate extend of indefinite arrays.

    friend const Expr* dock(std::unique_ptr<const Expr>& dst, const Expr* src) {
        if (src) {
            assert(src->docker_ == nullptr);
            src->docker_ = &dst;
        }
        return src;
    }

    friend void insert(const Expr* nexpr, std::unique_ptr<const Expr>& nexpr_dock, const Expr* child) {
        nexpr_dock.reset(nexpr);
        swap(nexpr_dock, *child->docker_); // nexpr_dock -> *child->docker_, *child->docker -> nexpr
        nexpr->docker_ = child->docker_;
        child->docker_ = &nexpr_dock;
    }

    friend class Args;
    friend class CodeGen;
    friend class IfExpr;
    friend class InferSema;
    friend class TypeSema;
};

/// Use as mixin for anything which uses args: (expr_1, ..., expr_n)
class Args {
public:
    Args(Exprs&& args)
        : args_(std::move(args))
    {
        for (auto& arg : args)
            arg->docker_ = &arg;
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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class LiteralExpr : public Expr {
public:
    enum Kind {
#define IMPALA_LIT(itype, atype) LIT_##itype = Token::LIT_##itype,
#include "impala/tokenlist.h"
        LIT_bool,
    };

    LiteralExpr(Location location, Kind kind, thorin::Box box)
        : Expr(location)
        , kind_(kind)
        , box_(box)
    {}

    Kind kind() const { return kind_; }
    thorin::Box box() const { return box_; }
    uint64_t get_u64() const;
    PrimTypeKind literal2type() const;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

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

    bool is_lvalue() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;

    Symbols symbols_;
    mutable std::vector<char> values_;
};

class FnExpr : public Expr, public Fn {
public:
    FnExpr(Location location, ASTTypeParams&& ast_type_params, Params&& params, const Expr* body)
        : Expr(location)
        , Fn(std::move(ast_type_params), std::move(params), body)
    {}

    const FnType* fn_type() const override { return type()->as<FnType>(); }
    Symbol fn_symbol() const override { return Symbol("lambda"); }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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
    const ValueDecl* value_decl() const { return value_decl_; }

    bool is_lvalue() const override;
    void take_address() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;

    std::unique_ptr<const Path> path_;
    mutable const ValueDecl* value_decl_ = nullptr; ///< Declaration of the variable in use.
};

class PrefixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_PREFIX(tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    PrefixExpr(Location location, Kind kind, const Expr* rhs)
        : Expr(location)
        , kind_(kind)
        , rhs_(dock(rhs_, rhs))
    {}

    static const PrefixExpr* create(const Expr* rhs, const Kind kind) {
        auto deref = new PrefixExpr(rhs->location(), kind, nullptr);
        insert(deref, deref->rhs_, rhs);
        return deref;
    }

    static const PrefixExpr* create_deref(const Expr* rhs) { return create(rhs, MUL); }
    static const PrefixExpr* create_addrof(const Expr* rhs) { return create(rhs, AND); }

    const Expr* rhs() const { return rhs_.get(); }
    Kind kind() const { return kind_; }

    bool is_lvalue() const override;
    bool has_side_effect() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
    std::unique_ptr<const Expr> rhs_;
};

class InfixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) tok = Token:: tok,
#define IMPALA_INFIX(     tok, str, lprec, rprec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    InfixExpr(Location location, const Expr* lhs, Kind kind, const Expr* rhs)
        : Expr(location)
        , kind_(kind)
        , lhs_(dock(lhs_, lhs))
        , rhs_(dock(rhs_, rhs))
    {}

    Kind kind() const { return kind_; }
    const Expr* lhs() const { return lhs_.get(); }
    const Expr* rhs() const { return rhs_.get(); }

    bool has_side_effect() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
    std::unique_ptr<const Expr> lhs_;
    std::unique_ptr<const Expr> rhs_;
};

/**
 * Just for expr++ and expr--.
 * For indexing/function calls use @p MapExpr.
 */
class PostfixExpr : public Expr {
public:
    enum Kind {
        INC = Token::INC,
        DEC = Token::DEC
    };

    PostfixExpr(Location location, const Expr* lhs, Kind kind)
        : Expr(location)
        , kind_(kind)
        , lhs_(dock(lhs_, lhs))
    {}

    Kind kind() const { return kind_; }
    const Expr* lhs() const { return lhs_.get(); }

    bool has_side_effect() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;

    Kind kind_;
    std::unique_ptr<const Expr> lhs_;
};

class FieldExpr : public Expr {
public:
    FieldExpr(Location location, const Expr* lhs, const Identifier* identifier)
        : Expr(location)
        , lhs_(dock(lhs_, lhs))
        , identifier_(identifier)
    {}

    const Expr* lhs() const { return lhs_.get(); }
    const Identifier* identifier() const { return identifier_.get(); }
    Symbol symbol() const { return identifier()->symbol(); }
    const FieldDecl* field_decl() const { return field_decl_; }
    uint32_t index() const { return field_decl()->index(); }

    bool is_lvalue() const override;
    void take_address() const override;

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;

    std::unique_ptr<const Expr> lhs_;
    std::unique_ptr<const Identifier> identifier_;
    mutable const FieldDecl* field_decl_ = nullptr;

    friend class MapExpr; // remove this
};

class CastExpr : public Expr {
public:
    CastExpr(Location location, const Expr* src)
        : Expr(location)
        , src_(dock(src_, src))
    {}

    const Expr* src() const { return src_.get(); }

    bool is_lvalue() const override;

    std::ostream& stream(std::ostream&) const override;

private:
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;

protected:
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

private:
    const Type* check(InferSema&) const override;

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
        auto implicit_cast_expr = new ImplicitCastExpr(src, type);
        insert(implicit_cast_expr, implicit_cast_expr->src_, src);
        return implicit_cast_expr;
    }

    void check(NameSema&) const override { THORIN_UNREACHABLE; }

private:
    const Type* check(InferSema&) const override;
};

class DefiniteArrayExpr : public Expr, public Args {
public:
    DefiniteArrayExpr(Location location, Exprs&& args)
        : Expr(location)
        , Args(std::move(args))
    {}

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
};

class StructExpr : public Expr {
public:
    class Elem : public ASTNode {
    public:
        Elem(Location location, const Identifier* identifier, const Expr* expr)
            : ASTNode(location)
            , identifier_(identifier)
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
    size_t num_elems() const { return elems_.size(); }
    const Elems& elems() const { return elems_; }

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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
        auto type_app_expr = new TypeAppExpr(lhs->location(), lhs, ASTTypes());
        insert(type_app_expr, type_app_expr->lhs_, lhs);
        return type_app_expr;
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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    bool is_lvalue() const override;
    bool has_side_effect() const override;
    void take_address() const override;
    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

private:
    const Type* check(InferSema&) const override;
    void check(TypeSema&) const override;
    thorin::Value lemit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&, State, Location) const;

    std::unique_ptr<const Expr> lhs_;

    friend class CodeGen;
    friend class ForExpr;
    friend class TypeSema;
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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    const thorin::Def* remit(CodeGen&) const override;
    void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void emit(CodeGen&, const thorin::Def*) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void emit(CodeGen&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void emit(CodeGen&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void emit(CodeGen&) const override;

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

    std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const override;
    void check(InferSema&) const override;
    void check(TypeSema&) const override;
    void emit(CodeGen&) const override;

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
