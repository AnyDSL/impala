#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/util/array.h"
#include "thorin/util/assert.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/cast.h"
#include "thorin/util/location.h"
#include "thorin/util/types.h"

#include "impala/impala.h"
#include "impala/symbol.h"
#include "impala/token.h"
#include "impala/sema/type.h"

namespace thorin {
    class Enter;
    class JumpTarget;
    class Continuation;
    class Param;
}

namespace impala {

class ASTType;
class Decl;
class Expr;
class Fn;
class FnDecl;
class Item;
class MapExpr;
class NamedItem;
class Stmt;
class ASTTypeParam;

class NameSema;
class InferSema;
class BorrowSema;
class TypeSema;
class CodeGen;

template<class T> using AutoPtr    = thorin::AutoPtr<T>;
template<class T> using AutoVector = thorin::AutoVector<T>;

typedef AutoVector<const ASTType*> ASTTypes;
typedef thorin::HashMap<Symbol, const FnDecl*> MethodTable;

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

    Visibility() {}
    Visibility(int visibility)
        : visibility_(visibility)
    {}

    const char* str();
    bool is_pub() const { return visibility_ == Pub; }
    bool is_priv() const { return visibility_ == Priv; }

private:
    int visibility_;

    friend class Parser;
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
    size_t num_ast_type_params() const { return ast_type_params_.size(); }
    const ASTTypeParam* ast_type_param(size_t i) const { return ast_type_params_[i]; }
    ArrayRef<const ASTTypeParam*> ast_type_params() const { return ast_type_params_; }
    std::ostream& stream_ast_type_params(std::ostream&) const;

protected:
    void check_ast_type_params(NameSema&) const;
    void check_ast_type_params(BorrowSema&) const;
    void check_ast_type_params(InferSema&) const;
    void check_ast_type_params(TypeSema&) const;

    AutoVector<const ASTTypeParam*> ast_type_params_;
};

//------------------------------------------------------------------------------

class ASTNode : public thorin::HasLocation, public thorin::Streamable, public thorin::MagicCast<ASTNode> {
public:
#ifndef NDEBUG
    virtual ~ASTNode() { assert(loc_.is_set()); }
#endif
};

template<typename... Args>
std::ostream& warning(const ASTNode* n, const char* fmt, Args... args) { return warning(n->loc(), fmt, args...); }
template<typename... Args>
std::ostream& error  (const ASTNode* n, const char* fmt, Args... args) { return error  (n->loc(), fmt, args...); }

//------------------------------------------------------------------------------

class Identifier : public ASTNode {
public:
    Identifier() {}
    Identifier(const char* str, const thorin::Location& loc)
        : symbol_(str)
    {
        loc_ = loc;
    }
    Identifier(Token tok)
        : symbol_(tok.symbol())
    {
        loc_ = tok.loc();
    }

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
        const Identifier* identifier() const { return identifier_; }
        Symbol symbol() const { return identifier()->symbol(); }
        const Decl* decl() const { return decl_; }
        virtual std::ostream& stream(std::ostream&) const override;
        void check(NameSema&) const;
        void check(BorrowSema&) const;

    private:
        AutoPtr<const Identifier> identifier_;
        mutable const Decl* decl_ = nullptr;

        friend class Parser;
        friend class Path;
    };

    typedef AutoVector<const Elem*> Elems;

    Path() {}
    Path(const Identifier* identifier) { // HACK
        auto elem = new Elem();
        elem->identifier_ = identifier;
        elems_.push_back(elem);
        elem->set_loc(identifier->loc());
        set_loc(identifier->loc());
    }

    bool is_global() const { return is_global_; }
    const Elems& elems() const { return elems_; }
    const Decl* decl() const { return elems().back()->decl(); }
    virtual std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const;
    void check(BorrowSema&) const;

private:
    bool is_global_;
    Elems elems_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * AST types
 */

class ASTType : public ASTNode, public Typeable {
public:
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class NameSema;
    friend class InferSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(const thorin::Location& loc) { loc_ = loc; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
};

class PrimASTType : public ASTType {
public:
    enum Kind {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    Kind kind() const { return kind_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;

    friend class Parser;
};

class PtrASTType : public ASTType {
public:
    enum Kind { Borrowed, Mut, Owned };

    Kind kind() const { return kind_; }
    std::string prefix() const;
    const ASTType* referenced_ast_type() const { return referenced_ast_type_; }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;
    int addr_space_;
    AutoPtr<const ASTType> referenced_ast_type_;

    friend class Parser;
};

class ArrayASTType : public ASTType {
public:
    const ASTType* elem_ast_type() const { return elem_ast_type_; }

protected:
    AutoPtr<const ASTType> elem_ast_type_;

    friend class Parser;
};

class IndefiniteArrayASTType : public ArrayASTType {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    uint64_t dim() const { return dim_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    thorin::u64 dim_;

    friend class Parser;
};

class CompoundASTType : public ASTType {
public:
    size_t num_ast_type_args() const { return ast_type_args_.size(); }
    ArrayRef<const ASTType*> ast_type_args() const { return ast_type_args_; }
    const ASTType* ast_type_arg(size_t i) const { return ast_type_args_[i]; }

protected:
    ASTTypes ast_type_args_;
    mutable std::vector<const Type*> type_args_;

    friend class Parser;
};

class TupleASTType : public CompoundASTType {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
};

class ASTTypeApp : public CompoundASTType {
public:
    const Path* path() const { return path_; }
    const Identifier* identifier() const { return path()->elems().back()->identifier(); }
    Symbol symbol() const { return identifier()->symbol(); }
    const Decl* decl() const { return path()->decl(); }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Path> path_;
    mutable const Decl* decl_ = nullptr;

    friend class Parser;
    friend class NameScope;
};

class FnASTType : public ASTTypeParamList, public CompoundASTType {
public:
    FnASTType() {}
    FnASTType(const thorin::Location& loc) { set_loc(loc); }

    const FnASTType* ret_fn_ast_type() const;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    friend class Parser;
};

class Typeof : public ASTType {
public:
    const Expr* expr() const { return expr_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class SimdASTType : public ArrayASTType {
public:
    uint64_t size() const { return size_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    thorin::u64 size_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * declarations
 */

/// Base class for all entities which have a \p symbol_.
class Decl : virtual public ASTNode {
public:
    const Identifier* identifier() const { return identifier_; }
    Symbol symbol() const { return identifier_->symbol(); }
    size_t depth() const { return depth_; }
    const Decl* shadows() const { return shadows_; }

protected:
    AutoPtr<const Identifier> identifier_;

private:
    mutable const Decl* shadows_;
    mutable size_t depth_;

    friend class NameSema;
};

/// Base class for all declarations which must have a \p Type assigned.
class TypeableDecl : public Decl, public Typeable {};

/// Base class for all declarations which represent a type definition.
class TypeDecl : public TypeableDecl {};

/// Base class for all declarations which represent a value.
class ValueDecl : public TypeableDecl {
public:
    const ASTType* ast_type() const { return ast_type_; } ///< Original \p ASTType.
    bool is_mut() const { return is_mut_; }
    bool is_written() const { return is_written_; }
    void write() const { is_written_ = true; }
    bool is_anonymous() const { return symbol() == Symbol() || symbol().str()[0] == '<'; }
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual thorin::Value emit(CodeGen&, const thorin::Def* init) const = 0;

protected:
    AutoPtr<const ASTType> ast_type_;
    bool is_mut_ = false;
    mutable bool is_written_ = false;
    mutable thorin::Value value_;

    friend class Parser;
    friend class CodeGen;
};

/// Base class for all values which may be mutated within a function.
class LocalDecl : public ValueDecl {
public:
    LocalDecl(size_t handle)
        : handle_(handle)
    {}

    size_t handle() const { return handle_; }
    bool is_address_taken() const { return is_address_taken_; }
    const Fn* fn() const { return fn_; }
    void take_address() const { is_address_taken_ = true; }
    void check(NameSema&) const;
    void check(BorrowSema&) const;

private:
    const Type* check(InferSema&) const;
    void check(TypeSema&) const;
    virtual thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

protected:
    size_t handle_;
    mutable const Fn* fn_ = nullptr;
    mutable bool is_address_taken_ = false;

    friend class InferSema;
    friend class Parser;
    friend class TypeSema;
    friend class ValueDecl;
};

//------------------------------------------------------------------------------

/*
 * parameters and Fn
 */

class ASTTypeParam : public TypeDecl {
public:
    size_t num_bounds() const { return bounds().size(); }
    const ASTTypes& bounds() const { return bounds_; }
    int lambda_depth() const { return lambda_depth_; }
    const Var* var() const { return type()->as<Var>(); }

    void check(NameSema&) const;
    const Var* check(TypeSema&) const;
    void check(BorrowSema&) const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    const Var* check(InferSema&) const;

    ASTTypes bounds_;

public: // HACK
    mutable int lambda_depth_ = -1;

    friend class InferSema;
    friend class Parser;
    friend class TypeSema;
};

class Param : public LocalDecl {
public:
    Param(size_t handle)
        : LocalDecl(handle)
    {}

    static const Param* create(size_t var_handle, const Identifier*, const thorin::Location&, const ASTType* fn_type);

    friend class Fn;
    friend class Parser;
};

class Fn : public ASTTypeParamList {
public:
    const Param* param(size_t i) const { return params_[i]; }
    ArrayRef<const Param*> params() const { return params_; }
    size_t num_params() const { return params_.size(); }
    const Expr* body() const { return body_; }
    thorin::Continuation* continuation() const { return continuation_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Def* frame() const { return frame_; }
    std::ostream& stream_params(std::ostream& p, bool returning) const;
    void fn_check(NameSema&) const;
    const Type* check_body(TypeSema&) const;
    void fn_check(BorrowSema&) const;
    thorin::Continuation* emit_head(CodeGen&, const thorin::Location&) const;
    void emit_body(CodeGen&, const thorin::Location& loc) const;

    bool is_continuation() const { return is_continuation_; }

    virtual const FnType* fn_type() const = 0;
    virtual Symbol fn_symbol() const = 0;

protected:
    mutable thorin::Continuation* continuation_;
    AutoVector<const Param*> params_;
    mutable const thorin::Param* ret_param_ = nullptr;
    mutable const thorin::Def* frame_ = nullptr;

private:
    AutoPtr<const Expr> body_;
    bool is_continuation_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * items
 */

class ModContents : public ASTNode {
public:
    const AutoVector<const Item*>& items() const { return items_; }
    const thorin::HashMap<Symbol, const NamedItem*>& item_table() const { return item_table_; }
    virtual std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const;
    void check(BorrowSema&) const;
    void emit(CodeGen&) const;

private:
    void check(InferSema&) const;
    void check(TypeSema&) const;

    AutoVector<const Item*> items_;
    mutable thorin::HashMap<Symbol, const NamedItem*> item_table_;

    friend class Parser;
    friend class InferSema;
    friend class TypeSema;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;

private:
    virtual void check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual void emit_item(CodeGen&) const = 0;

    Visibility visibility_;
#ifndef NDEBUG
    mutable bool done_ = false;
#endif

    friend class Parser;
    friend class NameSema;
    friend class InferSema;
    friend class TypeSema;
    friend class CodeGen;
};

class NamedItem : public Item {
public:
    virtual const Identifier* item_identifier() const = 0;
    Symbol item_symbol() const { return item_identifier()->symbol(); }
};

class TypeDeclItem : public NamedItem, public TypeDecl, public ASTTypeParamList {
public:
    virtual const Identifier* item_identifier() const override { return TypeDecl::identifier(); }

    friend class Parser;
};

class ValueItem : public NamedItem, public ValueDecl {
public:
    virtual const Identifier* item_identifier() const override { return ValueDecl::identifier(); }

private:
    virtual void emit_item(CodeGen&) const override;

    friend class Parser;
};

class ModDecl : public TypeDeclItem {
public:
    const ModContents* mod_contents() const { return mod_contents_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoPtr<const ModContents> mod_contents_;

    friend class Parser;
};

class ExternBlock : public Item {
public:
    Symbol abi() const { return abi_; }
    const AutoVector<const FnDecl*>& fns() const { return fns_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    Symbol abi_;
    AutoVector<const FnDecl*> fns_;

    friend class Parser;
};

class Typedef : public TypeDeclItem {
public:
    const ASTType* ast_type() const { return ast_type_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoPtr<const ASTType> ast_type_;

    friend class Parser;
};

class FieldDecl : public TypeableDecl {
public:
    uint32_t index() const { return index_; }
    const ASTType* ast_type() const { return ast_type_; }
    Visibility visibility() const { return  visibility_; }

    void check(NameSema&) const;
    void check(BorrowSema&) const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    const Type* check(InferSema&) const;
    void check(TypeSema&) const;

    uint32_t index_ = uint32_t(-1);
    AutoPtr<const ASTType> ast_type_;
    Visibility visibility_;

    friend class InferSema;
    friend class Parser;
    friend class TypeSema;
};

class StructDecl : public TypeDeclItem {
public:
    size_t num_field_decls() const { return field_decls_.size(); }
    const AutoVector<const FieldDecl*>& field_decls() const { return field_decls_; }
    const thorin::HashMap<Symbol, const FieldDecl*>& field_table() const { return field_table_; }
    const FieldDecl* field_decl(size_t i) const { return field_decls_[i]; }
    const FieldDecl* field_decl(Symbol symbol) const { return thorin::find(field_table_, symbol); }
    const FieldDecl* field_decl(const Identifier* ident) const { return field_decl(ident->symbol()); }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoVector<const FieldDecl*> field_decls_;
    mutable thorin::HashMap<Symbol, const FieldDecl*> field_table_;

    friend class Parser;
};

class EnumDecl : public TypeDeclItem {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;
};

class StaticItem : public ValueItem {
public:
    const Expr* init() const { return init_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    AutoPtr<const Expr> init_;

    friend class Parser;
};

class FnDecl : public ValueItem, public Fn {
public:
    bool is_extern() const { return is_extern_; }
    virtual const FnType* fn_type() const override {
        auto t = type();
        while (auto lambda = t->isa<Lambda>())
            t = lambda->body();
        return t->as<FnType>();
    }
    virtual Symbol fn_symbol() const override { return export_name_ ? export_name_->symbol() : identifier()->symbol(); }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value emit(CodeGen&, const thorin::Def* init) const override;

    AutoPtr<const Identifier> export_name_;
    bool is_extern_ = false;

    friend class Parser;
    friend class InferSema;
    friend class TypeSema;
};

class TraitDecl : public NamedItem, public Decl, public ASTTypeParamList {
public:
    const AutoVector<const ASTTypeApp*>& super_traits() const { return super_traits_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    const MethodTable& method_table() const { return method_table_; }

    virtual const Identifier* item_identifier() const override { return Decl::identifier(); }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoVector<const FnDecl*> methods_;
    AutoVector<const ASTTypeApp*> super_traits_;
    mutable MethodTable method_table_;

    friend class Parser;
};

class ImplItem : public Item, public ASTTypeParamList {
public:
    /// May be nullptr as trait is optional.
    const ASTType* trait() const { return trait_; }
    const ASTType* ast_type() const { return ast_type_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    const FnDecl* method(size_t i) const { return methods_[i]; }
    size_t num_methods() const { return methods_.size(); }
    const thorin::Def* def() const { return def_; }

    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoPtr<const ASTType> trait_;
    AutoPtr<const ASTType> ast_type_;
    AutoVector<const FnDecl*> methods_;
    mutable const thorin::Def* def_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode, public Typeable {
public:
#ifndef NDEBUG
    virtual ~Expr() { assert(docker_ != nullptr); }
#endif

    const thorin::Def* extra() const { return extra_; }

    virtual bool is_lvalue() const { return false; }
    virtual bool has_side_effect() const { return false; }
    virtual void take_address() const {}

    virtual std::ostream& stream(std::ostream&) const = 0;
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;

private:
    virtual const Type* check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual thorin::Value lemit(CodeGen&) const;
    virtual const thorin::Def* remit(CodeGen&) const;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const;

    mutable AutoPtr<const Expr>* docker_ = nullptr;

protected:
    mutable const thorin::Def* extra_ = nullptr; ///< Needed to propagate extend of indefinite arrays.

    friend void dock(AutoPtr<const Expr>& dst, const Expr* src) {
        assert(src->docker_ == nullptr);
        dst = src;
        src->docker_ = &dst;
    }

    friend void insert(const Expr* nexpr, AutoPtr<const Expr>& nexpr_dock, const Expr* child) {
        nexpr_dock = nexpr;
        swap(nexpr_dock, *child->docker_); // nexpr_dock -> *child->docker_, *child->docker -> nexpr
        nexpr->docker_ = child->docker_;
        child->docker_ = &nexpr_dock;
    }

    friend class CodeGen;
    friend class IfExpr;
    friend class InferSema;
    friend class Parser;
    friend class TypeSema;
};

/// Use as mixin for anything which uses args: (expr_1, ..., expr_n)
class Args {
public:
    const std::deque<AutoPtr<const Expr>>& args() const { return args_; }
    const Expr* arg(size_t i) const { assert(i < args_.size()); return args_[i]; }
    size_t num_args() const { return args_.size(); }
    std::ostream& stream_args(std::ostream& p) const;
    void append(const Expr* expr) {
        args_.emplace_back(nullptr);
        dock(args_.back(), expr);
    }

protected:
    std::deque<AutoPtr<const Expr>> args_;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const thorin::Location& loc) { loc_ = loc; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
};

class LiteralExpr : public Expr {
public:
    enum Kind {
#define IMPALA_LIT(itype, atype) LIT_##itype = Token::LIT_##itype,
#include "impala/tokenlist.h"
        LIT_bool,
    };

    LiteralExpr(const thorin::Location& loc, Kind kind, thorin::Box box)
        : kind_(kind)
        , box_(box)
    {
        loc_= loc;
    }

    Kind kind() const { return kind_; }
    thorin::Box box() const { return box_; }
    uint64_t get_u64() const;
    PrimTypeKind literal2type() const;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;
    thorin::Box box_;
};

class CharExpr : public Expr {
public:
    CharExpr(const thorin::Location& loc, Symbol symbol, thorin::u8 value)
        : symbol_(symbol)
        , value_(value)
    {
        loc_ = loc;
    }

    Symbol symbol() const { return symbol_; }
    thorin::u8 value() const { return value_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Symbol symbol_;
    thorin::u8 value_;
};

class StrExpr : public Expr {
public:
    const std::vector<Symbol>& symbols() const { return symbols_; }
    const std::vector<thorin::u8>& values() const { return values_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    std::vector<Symbol> symbols_;
    mutable std::vector<thorin::u8> values_;

    friend class Parser;
};

class FnExpr : public Expr, public Fn {
public:
    virtual const FnType* fn_type() const override { return type()->as<FnType>(); }
    virtual Symbol fn_symbol() const override { return Symbol("lambda"); }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    size_t ret_var_handle_;

    friend class Parser;
};

class PathExpr : public Expr {
public:
    PathExpr(const Path* path)
        : path_(path)
    {
        set_loc(path->loc());
    }

    const Path* path() const { return path_; }
    const ValueDecl* value_decl() const { return value_decl_; }

    virtual bool is_lvalue() const override;
    virtual void take_address() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value lemit(CodeGen&) const override;

    AutoPtr<const Path> path_;
    mutable const ValueDecl* value_decl_ = nullptr; ///< Declaration of the variable in use.

    friend class Parser;
};

class PrefixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_PREFIX(tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    static const PrefixExpr* create(const Expr* child, const Kind kind);
    static const PrefixExpr* create_deref(const Expr* child) { return create(child, MUL); }
    static const PrefixExpr* create_addrof(const Expr* child) { return create(child, AND); }

    const Expr* rhs() const { return rhs_; }
    Kind kind() const { return kind_; }

    virtual bool is_lvalue() const override;
    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Value lemit(CodeGen&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;
    AutoPtr<const Expr> rhs_;

    friend class Parser;
};

class InfixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) tok = Token:: tok,
#define IMPALA_INFIX(     tok, str, lprec, rprec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    Kind kind() const { return kind_; }
    const Expr* lhs() const { return lhs_; }
    const Expr* rhs() const { return rhs_; }

    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;
    AutoPtr<const Expr> lhs_;
    AutoPtr<const Expr> rhs_;

    friend class Parser;
};

/**
 * Just for expr++ and expr--.
 * For indexing/function calls use \p MapExpr.
 */
class PostfixExpr : public Expr {
public:
    enum Kind {
        INC = Token::INC,
        DEC = Token::DEC
    };

    Kind kind() const { return kind_; }
    const Expr* lhs() const { return lhs_; }

    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    Kind kind_;
    AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class FieldExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const Identifier* identifier() const { return identifier_; }
    Symbol symbol() const { return identifier()->symbol(); }
    const FieldDecl* field_decl() const { return field_decl_; }
    uint32_t index() const { return field_decl()->index(); }

    virtual bool is_lvalue() const override;
    virtual void take_address() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value lemit(CodeGen&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const Expr> lhs_;
    AutoPtr<const Identifier> identifier_;
    mutable const FieldDecl* field_decl_ = nullptr;

    friend class Parser;
    friend class MapExpr; // remove this
};

class CastExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }

    virtual bool is_lvalue() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

protected:
    AutoPtr<const Expr> lhs_;
};

class ExplicitCastExpr : public CastExpr {
public:
    const ASTType* ast_type() const { return ast_type_; }

    virtual void check(NameSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;

    AutoPtr<const ASTType> ast_type_;

    friend class Parser;
};

class ImplicitCastExpr : public CastExpr {
public:
    static const ImplicitCastExpr* create(const Expr*, const Type*);

    virtual void check(NameSema&) const override { THORIN_UNREACHABLE; }

private:
    virtual const Type* check(InferSema&) const override;
};

class DefiniteArrayExpr : public Expr, public Args {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    friend class Parser;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    const Expr* value() const { return value_; }
    thorin::u64 count() const { return count_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const Expr> value_;
    thorin::u64 count_;

    friend class Parser;
};

class IndefiniteArrayExpr : public Expr {
public:
    const Expr* dim() const { return dim_; }
    const ASTType* elem_ast_type() const { return elem_ast_type_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const Expr> dim_;
    AutoPtr<const ASTType> elem_ast_type_;

    friend class Parser;
};

class TupleExpr : public Expr, public Args {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    friend class Parser;
};

class SimdExpr : public Expr, public Args {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    friend class Parser;
};

class StructExpr : public Expr {
public:
    class Elem {
    public:
        Elem(const Elem&) = delete;
        Elem& operator=(Elem) = delete;

        Elem(const Identifier* identifier, const Expr* expr)
            : identifier_(identifier)
        {
            dock(expr_, expr);
        }

        const Identifier* identifier() const { return identifier_; }
        Symbol symbol() const { return identifier()->symbol(); }
        const Expr* expr() const { return expr_; }
        const FieldDecl* field_decl() const { return field_decl_; }

    private:
        AutoPtr<const Identifier> identifier_;
        AutoPtr<const Expr> expr_;
        mutable const FieldDecl* field_decl_ = nullptr;

        friend class StructExpr;
    };

    const ASTTypeApp* ast_type_app() const { return ast_type_app_; }
    size_t num_elems() const { return elems_.size(); }
    const std::deque<Elem>& elems() const { return elems_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const ASTTypeApp> ast_type_app_;
    std::deque<Elem> elems_;

    friend class Parser;
};

class TypeAppExpr : public Expr {
public:
    static const TypeAppExpr* create(const Expr*);

    const Expr* lhs() const { return lhs_; }
    const ASTTypes& ast_type_args() const { return ast_type_args_; }
    const ASTType* ast_type_arg(size_t i) const { assert(i < ast_type_args_.size()); return ast_type_args_[i]; }
    size_t num_ast_type_args() const { return ast_type_args().size(); }
    Types type_args() const { return type_args_; }
    const Type*& type_arg(size_t i) const { return type_args_[i]; }
    size_t num_type_args() const { return type_args_.size(); }
    std::ostream& stream_ast_type_args(std::ostream& p) const;
    std::ostream& stream_type_args(std::ostream& p) const;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value lemit(CodeGen&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const Expr> lhs_;
    ASTTypes ast_type_args_;
    mutable std::vector<const Type*> type_args_;

    friend class Parser;
};

class MapExpr : public Expr, public Args {
public:
    enum State {
        None, Run, Hlt
    };

    const Expr* lhs() const { return lhs_; }

    virtual bool is_lvalue() const override;
    virtual bool has_side_effect() const override;
    virtual void take_address() const override;
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual thorin::Value lemit(CodeGen&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
    const thorin::Def* remit(CodeGen&, State, thorin::Location) const;

    AutoPtr<const Expr> lhs_;

    friend class CodeGen;
    friend class Parser;
    friend class ForExpr;
    friend class TypeSema;
};

class StmtLikeExpr : public Expr {};

class BlockExprBase : public StmtLikeExpr {
public:
    const AutoVector<const Stmt*>& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_; }
    const Stmt* stmt(size_t i) const { return stmts_[i]; }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    const std::vector<const LocalDecl*>& locals() const { return locals_; }
    void add_local(const LocalDecl* local) const { locals_.push_back(local); }

    virtual const char* prefix() const = 0;
    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

protected:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoVector<const Stmt*> stmts_;
    AutoPtr<const Expr> expr_;
    mutable std::vector<const LocalDecl*> locals_; ///< All \p LocalDecl%s in this \p BlockExprBase from top to bottom.

    friend class Parser;
};

class BlockExpr : public BlockExprBase {
public:
    BlockExpr() {}
    BlockExpr(thorin::Location loc) { loc_ = loc; dock(expr_, new EmptyExpr(loc)); }

    virtual const char* prefix() const override { return "{"; }

    friend class Parser;
};

class RunBlockExpr : public BlockExprBase {
public:
    virtual const char* prefix() const override { return "@{"; }

private:
    virtual const thorin::Def* remit(CodeGen&) const override;

    friend class Parser;
};

class IfExpr : public StmtLikeExpr {
public:
    const Expr* cond() const { return cond_; }
    const Expr* then_expr() const { return then_expr_; }
    const Expr* else_expr() const { return else_expr_; }
    bool has_else() const;

    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Expr> cond_;
    AutoPtr<const Expr> then_expr_;
    AutoPtr<const Expr> else_expr_;

    friend class Parser;
};

class WhileExpr : public StmtLikeExpr {
public:
    const Expr* cond() const { return cond_; }
    const BlockExprBase* body() const { return body_->as<BlockExprBase>(); }
    const LocalDecl* break_decl() const { return break_decl_; }
    const LocalDecl* continue_decl() const { return continue_decl_; }

    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Expr> cond_;
    AutoPtr<const Expr> body_;
    AutoPtr<const LocalDecl> break_decl_;
    AutoPtr<const LocalDecl> continue_decl_;

    friend class Parser;
};

class ForExpr : public StmtLikeExpr {
public:
    const FnExpr* fn_expr() const { return fn_expr_->as<FnExpr>(); }
    const Expr* expr() const { return expr_; }
    const LocalDecl* break_decl() const { return break_decl_; }

    virtual bool has_side_effect() const override;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual const Type* check(InferSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual const thorin::Def* remit(CodeGen&) const override;

    AutoPtr<const Expr> fn_expr_;
    AutoPtr<const Expr> expr_;
    AutoPtr<const LocalDecl> break_decl_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * statements
 */

class Stmt : public ASTNode {
public:
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;

private:
    virtual void check(InferSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class InferSema;
    friend class TypeSema;
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    virtual void check(InferSema&) const override;
    virtual void check(TypeSema&) const override;

    AutoPtr<const LocalDecl> local_;
    AutoPtr<const Expr> init_;

    friend class Parser;
};

//------------------------------------------------------------------------------

}

#endif
