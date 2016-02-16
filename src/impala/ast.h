#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/util/array.h"
#include "thorin/util/assert.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/cast.h"
#include "thorin/util/location.h"
#include "thorin/util/stream.h"
#include "thorin/util/types.h"

#include "impala/symbol.h"
#include "impala/token.h"
#include "impala/sema/unifiable.h"

namespace thorin {
    class Enter;
    class JumpTarget;
    class Lambda;
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
class TypeParam;

class CodeGen;
class NameSema;
class BorrowSema;
class TypeSema;

template<class T> using SafePtr    = thorin::SafePtr<T>;
template<class T> using AutoPtr    = thorin::AutoPtr<T>;
template<class T> using AutoVector = thorin::AutoVector<T>;

typedef AutoVector<const ASTType*> ASTTypes;
typedef AutoVector<const Expr*> Exprs;
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
    Type type() const { return type_; }

protected:
    mutable Type type_;

    friend class TypeSema;
};

/// Mixin for all entities which have a list of \p TypeParam%s: [T1, T2 : A + B[...], ...].
class TypeParamList {
public:
    const TypeParam* type_param(size_t i) const { return type_params_[i]; }
    ArrayRef<const TypeParam*> type_params() const { return type_params_; }
    std::ostream& stream_type_params(std::ostream&) const;

protected:
    void check_type_params(NameSema&) const;
    void check_type_params(BorrowSema&) const;
    void check_type_params(TypeSema&) const;

    AutoVector<const TypeParam*> type_params_;
};

/// a bundle of expected type and a flag whether noret is also allowed
class TypeExpectation {
public:
    TypeExpectation(Type type)
        : type_(type)
        , noret_(false)
        , what_()
    {}
    TypeExpectation(Type type, bool noret)
        : type_(type)
        , noret_(noret)
        , what_()
    {}
    TypeExpectation(Type type, const std::string& what)
        : type_(type)
        , noret_(false)
        , what_(what)
    {}
    TypeExpectation(Type type, bool noret, const std::string& what)
        : type_(type)
        , noret_(noret)
        , what_(what)
    {}
    TypeExpectation(TypeExpectation ty_exp, const std::string& what)
        : type_(ty_exp.type())
        , noret_(ty_exp.noret())
        , what_(what)
    {}
    TypeExpectation(TypeExpectation ty_exp, Type type)
        : type_(type)
        , noret_(ty_exp.noret())
        , what_(ty_exp.what())
    {}
    /// The expected type
    Type type() const { return type_; }
    /// Indicates whether NoRet is also allowed
    bool noret() const { return noret_; }
    const std::string& what() const { return what_; }
private:
    const Type type_;
    const bool noret_;
    const std::string what_;
};

//------------------------------------------------------------------------------

class ASTNode : public thorin::HasLocation, public thorin::Streamable, public thorin::MagicCast<ASTNode> {
public:
#ifndef NDEBUG
    virtual ~ASTNode() { assert(loc_.is_set()); }
#endif
};

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
    virtual std::ostream& stream(std::ostream&) const override;

private:
    Symbol symbol_;
};

/*
 * paths
 */

class PathElem : public ASTNode {
public:
    const Identifier* identifier() const { return identifier_; }
    Symbol symbol() const { return identifier()->symbol(); }
    SafePtr<const Decl> decl() const { return decl_; }
    virtual std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const;
    void check(BorrowSema&) const;

private:
    AutoPtr<const Identifier> identifier_;
    mutable SafePtr<const Decl> decl_;

    friend class Parser;
};

typedef AutoVector<const PathElem*> PathElems;

class Path : public ASTNode {
public:
    bool is_global() const { return is_global_; }
    const PathElems& path_elems() const { return path_elems_; }
    virtual std::ostream& stream(std::ostream&) const override;
    void check(NameSema&) const;
    void check(BorrowSema&) const;
    SafePtr<const Decl> decl() const { return path_elems_.back()->decl(); }

private:
    bool is_global_;
    PathElems path_elems_;

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
    virtual Type check(TypeSema&) const = 0;

    friend class NameSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(const thorin::Location& loc) { loc_ = loc; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
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
    virtual Type check(TypeSema&) const override;

    Kind kind_;

    friend class Parser;
};

class PtrASTType : public ASTType {
public:
    char kind() const { assert(is_owned() || is_borrowed()); return kind_; }
    const ASTType* referenced_type() const { return referenced_type_; }
    bool is_owned() const { return kind_ == '~'; }
    bool is_borrowed() const { return kind_ == '&'; }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;

    char kind_;
    int addr_space_;
    AutoPtr<const ASTType> referenced_type_;

    friend class Parser;
};

class ArrayASTType : public ASTType {
public:
    const ASTType* elem_type() const { return elem_type_; }

protected:
    AutoPtr<const ASTType> elem_type_;

    friend class Parser;
};

class IndefiniteArrayASTType : public ArrayASTType {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    uint64_t dim() const { return dim_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;

    thorin::u64 dim_;

    friend class Parser;
};

class CompoundASTType : public ASTType {
public:
    size_t num_args() const { return args_.size(); }
    ArrayRef<const ASTType*> args() const { return args_; }
    const ASTType* arg(size_t i) const { return args_[i]; }

protected:
    ASTTypes args_;

    friend class Parser;
};

class TupleASTType : public CompoundASTType {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
};

class ASTTypeApp : public CompoundASTType {
public:
    const Identifier* identifier() const { return identifier_; }
    Symbol symbol() const { return identifier()->symbol(); }
    SafePtr<const Decl> decl() const { return decl_; }
    TraitApp trait_app(TypeSema&, Type) const;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;

    AutoPtr<const Identifier> identifier_;
    mutable SafePtr<const Decl> decl_;

    friend class Parser;
    friend class NameScope;
};

class FnASTType : public TypeParamList, public CompoundASTType {
public:
    FnASTType() {}
    FnASTType(const thorin::Location& loc) { set_loc(loc); }

    const FnASTType* ret_fn_type() const;

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;

    friend class Parser;
};

class Typeof : public ASTType {
public:
    const Expr* expr() const { return expr_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;

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
    virtual Type check(TypeSema&) const override;

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
class TypeableDecl : public Decl, public Typeable {
private:
    virtual Type check(TypeSema&) const = 0;

    friend class NameSema;
    friend class TypeSema;
    friend class ForExpr;
};

/// Base class for all declarations which represent a type definition.
class TypeDecl : public TypeableDecl {
};

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
    virtual Type check(TypeSema& sema) const override;
    Type check(TypeSema&, Type) const;
    virtual thorin::Var emit(CodeGen&, thorin::Def init) const = 0;

protected:
    AutoPtr<const ASTType> ast_type_;
    bool is_mut_ = false;
    mutable bool is_written_ = false;
    mutable thorin::Var var_;

    friend class Parser;
    friend class TypeSema;
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
    const Expr* fn() const { return fn_; }
    void take_address() const { is_address_taken_ = true; }
    void check(NameSema&) const;
    void check(BorrowSema&) const;

private:
    virtual thorin::Var emit(CodeGen&, thorin::Def init) const override;

protected:
    size_t handle_;
    mutable const Expr* fn_ = nullptr;
    mutable bool is_address_taken_ = false;

    friend class Parser;
    friend class ValueDecl;
};

//------------------------------------------------------------------------------

/*
 * parameters and Fn
 */

class TypeParam : public TypeDecl {
public:
    size_t num_bounds() const { return bounds().size(); }
    const ASTTypes& bounds() const { return bounds_; }
    TypeVar type_var() const { return type().as<TypeVar>(); }
    TypeVar type_var(TypeSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(BorrowSema&) const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual Type check(TypeSema&) const override;

    ASTTypes bounds_;

    friend class Parser;
};

class SelfParam : public TypeParam {
public:
    SelfParam(const thorin::Location&) {}
    void set_loc(const thorin::Location& loc) { loc_ = loc; set_identifier(loc); }

private:
    void set_identifier(const thorin::Location& loc) { identifier_ = new Identifier("Self", loc); }
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

class Fn : public TypeParamList {
public:
    const Param* param(size_t i) const { return params_[i]; }
    ArrayRef<const Param*> params() const { return params_; }
    size_t num_params() const { return params_.size(); }
    const Expr* body() const { return body_; }
    thorin::Lambda* lambda() const { return lambda_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    thorin::Def frame() const { return frame_; }
    std::ostream& stream_params(std::ostream& p, bool returning) const;
    void fn_check(NameSema&) const;
    void fn_check(BorrowSema&) const;
    void check_body(TypeSema&, FnType) const;
    thorin::Lambda* emit_head(CodeGen&, const thorin::Location&) const;
    void emit_body(CodeGen&, const thorin::Location& loc) const;

    bool is_continuation() const { return cont_; }

    virtual FnType fn_type() const = 0;
    virtual Symbol fn_symbol() const = 0;

protected:
    mutable thorin::Lambda* lambda_;
    mutable SafePtr<const thorin::Param> ret_param_;
    mutable thorin::Def frame_;
    AutoVector<const Param*> params_;

private:
    AutoPtr<const Expr> body_;
    bool cont_;

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
    void check(TypeSema&) const;
    void emit(CodeGen&) const;

private:
    AutoVector<const Item*> items_;
    mutable thorin::HashMap<Symbol, const NamedItem*> item_table_;

    friend class Parser;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;
    virtual void check_item(TypeSema&) const = 0;

private:
    virtual void emit_item(CodeGen&) const = 0;

    Visibility visibility_;
#ifndef NDEBUG
    mutable bool done_ = false;
#endif

    friend class CodeGen;
    friend class Parser;
    friend class NameSema;
    friend class TypeSema;
};

class NamedItem : public Item {
public:
    virtual const Identifier* item_identifier() const = 0;
    Symbol item_symbol() const { return item_identifier()->symbol(); }
};

class TypeDeclItem : public NamedItem, public TypeDecl, public TypeParamList {
public:
    virtual const Identifier* item_identifier() const override { return TypeDecl::identifier(); }

private:
    virtual void check_item(TypeSema&) const override;

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
    virtual Type check(TypeSema&) const override;
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
    virtual void check_item(TypeSema&) const override;
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
    virtual Type check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;

    AutoPtr<const ASTType> ast_type_;

    friend class Parser;
};

class FieldDecl : public TypeDecl {
public:
    int index() const { return index_; }
    const ASTType* ast_type() const { return ast_type_; } ///< Original \p ASTType.
    Visibility visibility() const { return  visibility_; }
    void check(NameSema&) const;
    void check(BorrowSema&) const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual Type check(TypeSema&) const override;
    //virtual thorin::Var emit(CodeGen&) const override;

    int index_;
    AutoPtr<const ASTType> ast_type_;
    Visibility visibility_;

    friend class Parser;
};

class StructDecl : public TypeDeclItem {
public:
    size_t num_field_decls() const { return field_decls_.size(); }
    const AutoVector<const FieldDecl*>& field_decls() const { return field_decls_; }
    const thorin::HashMap<Symbol, const FieldDecl*>& field_table() const { return field_table_; }
    const FieldDecl* field_decl(Symbol symbol) const { return thorin::find(field_table_, symbol); }
    const FieldDecl* field_decl(const Identifier* ident) const { return field_decl(ident->symbol()); }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
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
    virtual Type check(TypeSema&) const override;
    virtual void emit_item(CodeGen&) const override;
};

class StaticItem : public ValueItem {
public:
    const Expr* init() const { return init_; }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void check_item(TypeSema&) const override;
    virtual thorin::Var emit(CodeGen&, thorin::Def init) const override;

    AutoPtr<const Expr> init_;

    friend class Parser;
};

class FnDecl : public ValueItem, public Fn {
public:
    bool is_extern() const { return is_extern_; }
    virtual FnType fn_type() const override { return type().as<FnType>(); }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual Symbol fn_symbol() const override { return export_name_ ? export_name_->symbol() : identifier()->symbol(); }

private:
    virtual Type check(TypeSema&) const override;
    virtual void check_item(TypeSema&) const override;
    virtual thorin::Var emit(CodeGen&, thorin::Def init) const override;

    AutoPtr<const Identifier> export_name_;
    bool is_extern_ = false;

    friend class Parser;
};

class TraitDecl : public NamedItem, public Decl, public TypeParamList {
public:
    TraitDecl()
        : self_param_(thorin::Location(loc().begin(), loc().begin()))
    {}

    const AutoVector<const ASTTypeApp*>& super_traits() const { return super_traits_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    const MethodTable& method_table() const { return method_table_; }
    const SelfParam* self_param() const { return &self_param_; }
    TraitAbs trait_abs() const { return trait_abs_; }
    virtual const Identifier* item_identifier() const override { return Decl::identifier(); }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void check_item(TypeSema&) const override;

private:
    virtual void emit_item(CodeGen&) const override;

    const SelfParam self_param_;
    AutoVector<const FnDecl*> methods_;
    AutoVector<const ASTTypeApp*> super_traits_;
    mutable MethodTable method_table_;
    mutable TraitAbs trait_abs_;

    friend class Parser;
};

class ImplItem : public Item, public TypeParamList {
public:
    /// May be nullptr as trait is optional.
    const ASTType* trait() const { return trait_; }
    const ASTType* ast_type() const { return ast_type_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    const FnDecl* method(size_t i) const { return methods_[i]; }
    size_t num_methods() const { return methods_.size(); }
    thorin::Def def() const { return def_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void check_item(TypeSema&) const override;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual void emit_item(CodeGen&) const override;

    AutoPtr<const ASTType> trait_;
    AutoPtr<const ASTType> ast_type_;
    AutoVector<const FnDecl*> methods_;
    mutable thorin::Def def_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode, public Typeable {
public:
    /// return the type before implicit casting (for example ~4 always has the actual_type ~int but its type could be &int due to subtyping)
    Type actual_type() const { return actual_type_.empty() ? type() : actual_type_; }
    bool needs_cast() const { return !actual_type_.empty(); }
    thorin::Def extra() const { return extra_; }
    virtual bool is_lvalue() const { return false; }
    virtual bool has_side_effect() const { return false; }
    virtual void take_address() const {}
    virtual void check(NameSema&) const = 0;
    virtual void check(BorrowSema&) const = 0;

private:
    virtual Type check(TypeSema&, TypeExpectation) const = 0;
    virtual thorin::Var lemit(CodeGen&) const;
    virtual thorin::Def remit(CodeGen&) const;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const;

    mutable Type actual_type_;

protected:
    mutable thorin::Def extra_; ///< Needed to propagate extend of indefinite arrays.

    friend class CodeGen;
    friend class Parser;
    friend class TypeSema;
    friend class IfExpr;
};

/// Use as mixin for anything which uses type args: [T1, ..., Tn]
class TypeArgs {
public:
    const ASTTypes& type_args() const { return type_args_; }
    const ASTType* type_arg(size_t i) const { assert(i < type_args_.size()); return type_args_[i]; }
    size_t num_type_args() const { return type_args_.size(); }
    ArrayRef<Type> inferred_args() const { return inferred_args_; }
    Type inferred_arg(size_t i) const { return inferred_args_[i]; }
    size_t num_inferred_args() const { return inferred_args_.size(); }
    std::ostream& stream_type_args(std::ostream& p) const;

protected:
    ASTTypes type_args_;
    mutable std::vector<Type> inferred_args_;
};

/// Use as mixin for anything which uses args: (expr_1, ..., expr_n)
class Args {
public:
    const Exprs& args() const { return args_; }
    const Expr* arg(size_t i) const { assert(i < args_.size()); return args_[i]; }
    size_t num_args() const { return args_.size(); }
    std::ostream& stream_args(std::ostream& p) const;

protected:
    Exprs args_;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const thorin::Location& loc) { loc_ = loc; }

    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;
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
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    Kind kind_;
    thorin::Box box_;
};

class CharExpr : public Expr {
public:
    CharExpr(const thorin::Location& loc, Symbol symbol)
        : symbol_(symbol)
    {
        loc_ = loc;
    }

    Symbol symbol() const { return symbol_; }
    thorin::u8 value() const { return value_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    Symbol symbol_;
    mutable thorin::u8 value_;
};

class StrExpr : public Expr {
public:
    const std::vector<Symbol>& symbols() const { return symbols_; }
    const std::vector<thorin::u8>& values() const { return values_; }
    bool is_used_as_global() const { return is_used_as_global_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    std::vector<Symbol> symbols_;
    mutable std::vector<thorin::u8> values_;
    mutable bool is_used_as_global_ = false;

    friend class Parser;
};

class FnExpr : public Expr, public Fn {
public:
    virtual FnType fn_type() const override { return type().as<FnType>(); }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual Symbol fn_symbol() const override { return Symbol("lambda"); }

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

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
    SafePtr<const ValueDecl> value_decl() const { return value_decl_; }
    virtual bool is_lvalue() const override;
    virtual void take_address() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Var lemit(CodeGen&) const override;

    AutoPtr<const Path> path_;
    mutable SafePtr<const ValueDecl> value_decl_; ///< Declaration of the variable in use.

    friend class Parser;
};

class PrefixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_PREFIX(tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    static const PrefixExpr* create_deref(const AutoPtr<const Expr>& dock);

    const Expr* rhs() const { return rhs_; }
    Kind kind() const { return kind_; }
    virtual bool is_lvalue() const override;
    virtual bool has_side_effect() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Var lemit(CodeGen&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

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
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

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
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    Kind kind_;
    AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class FieldExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const Identifier* identifier() const { return identifier_; }
    Symbol symbol() const { return identifier()->symbol(); }
    uint32_t index() const { return index_; }
    virtual bool is_lvalue() const override;
    virtual void take_address() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    Type check_as_struct(TypeSema&, Type) const;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Var lemit(CodeGen&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Expr> lhs_;
    AutoPtr<const Identifier> identifier_;
    mutable uint32_t index_ = uint32_t(-1);

    friend class Parser;
    friend class MapExpr; // remove this
};

class CastExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const ASTType* ast_type() const { return ast_type_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual bool is_lvalue() const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Expr> lhs_;
    AutoPtr<const ASTType> ast_type_;

    friend class Parser;
};

class DefiniteArrayExpr : public Expr, public Args {
public:
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    friend class Parser;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    const Expr* value() const { return value_; }
    thorin::u64 count() const { return count_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Expr> value_;
    thorin::u64 count_;

    friend class Parser;
};

class IndefiniteArrayExpr : public Expr {
public:
    const Expr* dim() const { return dim_; }
    const ASTType* elem_type() const { return elem_type_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Expr> dim_;
    AutoPtr<const ASTType> elem_type_;

    friend class Parser;
};

class TupleExpr : public Expr, public Args {
public:
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    friend class Parser;
};

class SimdExpr : public Expr, public Args {
public:
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    friend class Parser;
};

class StructExpr : public Expr, public TypeArgs {
public:
    class Elem {
    public:
        Elem(const Elem& elem)
            : identifier_(elem.identifier())
            , expr_(elem.expr())
        {}
        Elem(const Identifier* identifier, const Expr* expr)
            : identifier_(identifier)
            , expr_(expr)
        {}

        const Identifier* identifier() const { return identifier_; }
        Symbol symbol() const { return identifier()->symbol(); }
        const Expr* expr() const { return expr_; }
        const FieldDecl* field_decl() const { return field_decl_; }

    private:
        const Identifier* identifier_;
        const Expr* expr_;
        mutable SafePtr<const FieldDecl> field_decl_;

        friend class StructExpr;
    };

    virtual ~StructExpr() {
        for (auto elem : elems()) {
            delete elem.identifier_;
            delete elem.expr_;
        }
    }

    const Path* path() const { return path_; }
    size_t num_elems() const { return elems_.size(); }
    const std::vector<Elem>& elems() const { return elems_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Path> path_;
    std::vector<Elem> elems_;

    friend class Parser;
};

class MapExpr : public Expr, public Args, public TypeArgs {
public:
    enum State {
        None, Run, Hlt
    };

    const Expr* lhs() const { return lhs_; }
    FnType fn_mono() const { return fn_mono_; }
    virtual bool is_lvalue() const override;
    virtual bool has_side_effect() const override;
    virtual void take_address() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    Type check_as_map(TypeSema&, TypeExpectation) const;
    Type check_as_method_call(TypeSema&, TypeExpectation) const;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Var lemit(CodeGen&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    thorin::Def remit(CodeGen&, State, thorin::Location) const;

    AutoPtr<const Expr> lhs_;
    mutable FnType fn_mono_;

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
    virtual bool has_side_effect() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual const char* prefix() const = 0;

private:
    virtual std::ostream& stream(std::ostream&) const override;

protected:
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoVector<const Stmt*> stmts_;
    AutoPtr<const Expr> expr_;
    mutable std::vector<const LocalDecl*> locals_; ///< All \p LocalDecl%s in this \p BlockExprBase from top to bottom.

    friend class Parser;
};

class BlockExpr : public BlockExprBase {
public:
    BlockExpr() {}
    BlockExpr(thorin::Location loc) { loc_ = loc; expr_ = new EmptyExpr(loc); }

    virtual const char* prefix() const override { return "{"; }

    friend class Parser;
};

class RunBlockExpr : public BlockExprBase {
public:
    virtual const char* prefix() const override { return "@{"; }

private:
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    friend class Parser;
};

class IfExpr : public StmtLikeExpr {
public:
    const Expr* cond() const { return cond_; }
    const Expr* then_expr() const { return then_expr_; }
    const Expr* else_expr() const { return else_expr_; }
    bool has_else() const;
    virtual bool has_side_effect() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    AutoPtr<const Expr> cond_;
    AutoPtr<const Expr> then_expr_;
    AutoPtr<const Expr> else_expr_;

    friend class Parser;
};

class WhileExpr : public StmtLikeExpr {
public:
    const Expr* cond() const { return cond_; }
    const BlockExprBase* body() const { return body_; }
    const LocalDecl* break_decl() const { return break_decl_; }
    const LocalDecl* continue_decl() const { return continue_decl_; }
    virtual bool has_side_effect() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;

    AutoPtr<const Expr> cond_;
    AutoPtr<const BlockExprBase> body_;
    AutoPtr<const LocalDecl> break_decl_;
    AutoPtr<const LocalDecl> continue_decl_;

    friend class Parser;
};

class ForExpr : public StmtLikeExpr {
public:
    const FnExpr* fn_expr() const { return fn_expr_; }
    const Expr* expr() const { return expr_; }
    const LocalDecl* break_decl() const { return break_decl_; }
    virtual bool has_side_effect() const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;

private:
    virtual std::ostream& stream(std::ostream&) const override;
    virtual Type check(TypeSema&, TypeExpectation) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const FnExpr> fn_expr_;
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
    virtual void check(TypeSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }
    virtual std::ostream& stream(std::ostream&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    virtual std::ostream& stream(std::ostream&) const override;
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }
    virtual void check(NameSema&) const override;
    virtual void check(BorrowSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    AutoPtr<const LocalDecl> local_;
    AutoPtr<const Expr> init_;

    friend class Parser;
};

//------------------------------------------------------------------------------

}

#endif
