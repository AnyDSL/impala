#ifndef IMPALA_AST_H
#define IMPALA_AST_H

#include <memory>
#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/util/array.h"
#include "thorin/util/assert.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/cast.h"
#include "thorin/util/types.h"

#include "impala/location.h"
#include "impala/symbol.h"
#include "impala/token.h"
#include "impala/sema/type.h"

namespace thorin {
    class Enter;
    class JumpTarget;
    class Lambda;
}

namespace impala {

class ASTType;
class Decl;
class Expr;
class Item;
class Stmt;
class TypeParam;

class Printer;
class CodeGen;
class NameSema;
class TypeSema;

template<class T> using SafePtr    = thorin::SafePtr<T>;
template<class T> using AutoPtr    = thorin::AutoPtr<T>;
template<class T> using AutoVector = thorin::AutoVector<T>;
template<class T> using ArrayRef   = thorin::ArrayRef<T>;

typedef AutoVector<const ASTType*> ASTTypes;
typedef AutoVector<const Expr*> Exprs;

//------------------------------------------------------------------------------

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

class TypeParamList {
public:
    const TypeParam* type_param(size_t i) const { return type_params_[i]; }
    ArrayRef<const TypeParam*> type_params() const { return type_params_; }
    std::ostream& print_type_params(Printer&) const;

protected:
    void check_type_params(NameSema&) const;
    void check_type_params(TypeSema&) const;

    AutoVector<const TypeParam*> type_params_;
};

class ASTNode : public impala::HasLocation, public thorin::MagicCast<ASTNode> {
public:
#ifndef NDEBUG
    virtual ~ASTNode() { assert(loc_.is_set()); }
#endif
    virtual std::ostream& print(Printer&) const = 0;
    void dump() const;
};

//------------------------------------------------------------------------------

/*
 * paths
 */

class PathElem : public ASTNode {
public:
    Symbol symbol() const { return symbol_; }
    const ASTTypes& args() const { return args_; }
    SafePtr<const Decl> decl() const { return decl_; }
    virtual std::ostream& print(Printer&) const override;
    void check(NameSema&) const;

private:
    Symbol symbol_;
    ASTTypes args_;
    mutable SafePtr<const Decl> decl_;

    friend class Parser;
};

typedef AutoVector<const PathElem*> PathElems;

class Path : public ASTNode {
public:
    bool is_global() const { return is_global_; }
    const PathElems& path_elems() const { return path_elems_; }
    virtual std::ostream& print(Printer&) const override;
    void check(NameSema&) const;
    SafePtr<const Decl> decl() const { return path_elems_.back()->decl(); }

private:
    bool is_global_;
    PathElems path_elems_;

    friend class Parser;
};

/// Base class of all entities that have a type assigned. 
/// Use as a mixin.
class Typeable {
public:
    Type type() const { return type_; }

protected:
    mutable Type type_;
    friend class TypeSema;
};

/*
 * AST types
 */

class ASTType : public ASTNode, public Typeable {
private:
    virtual void check(NameSema&) const = 0;
    virtual Type check(TypeSema&) const = 0;

    friend class NameSema;
    friend class TypeSema;
};

class ErrorASTType : public ASTType {
public:
    ErrorASTType(const Location& loc) { loc_ = loc; }

    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;
};

class PrimASTType : public ASTType {
public:
    enum Kind {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    Kind kind() const { return kind_; }
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
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
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;

    char kind_;
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
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    uint64_t dim() const { return dim_; }
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;

    thorin::u64 dim_;

    friend class Parser;
};

class CompoundASTType : public ASTType {
public:
    ArrayRef<const ASTType*> elems() const { return elems_; }
    const ASTType* elem(size_t i) const { return elems_[i]; }

protected:
    ASTTypes elems_;

    friend class Parser;
};

class TupleASTType : public CompoundASTType {
public:
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;
};

class ASTTypeApp : public CompoundASTType {
public:
    Symbol symbol() const { return symbol_; }
    SafePtr<const Decl> decl() const { return decl_; }
    virtual std::ostream& print(Printer&) const override;
    Trait to_trait(TypeSema&, Type) const;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;

    Symbol symbol_;
    mutable SafePtr<const Decl> decl_;

    friend class Parser;
    friend class NameScope;
};

class FnASTType : public TypeParamList, public CompoundASTType {
public:
    const FnASTType* ret_fn_type() const;
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;

    friend class Parser;
};


//------------------------------------------------------------------------------

/*
 * declarations
 */

class Decl : virtual public ASTNode {
public:
    Symbol symbol() const { return symbol_; }
    size_t depth() const { return depth_; }
    const Decl* shadows() const { return shadows_; }

protected:
    Symbol symbol_;

private:
    mutable const Decl* shadows_;
    mutable size_t depth_;

    friend class NameSema;
};

/// Base class for all declarations which must have inferred a \p Type.
class TypeDecl : public Decl, public Typeable {
private:
    virtual void check(NameSema&) const = 0;
    virtual Type check(TypeSema&) const = 0;

    mutable bool checked_ = false;

    friend class NameSema;
    friend class TypeSema;
};

/// Base class for all declarations which have a type.
class ValueDecl : public TypeDecl {
public:
    const ASTType* ast_type() const { return ast_type_; } ///< Original \p ASTType.
    bool is_mut() const { return is_mut_; }
    virtual thorin::Var var(CodeGen&) const;
    virtual void check(NameSema&) const override;

private:
    Type check(TypeSema&, Type) const;
    virtual Type check(TypeSema& sema) const override;
    AutoPtr<const ASTType> ast_type_;
    bool is_mut_ = false;

protected:
    mutable thorin::Var var_;

    friend class Parser;
    friend class TypeSema;
    friend class ForExpr;
};

/// Base class for all Values which may be mutated within a function.
class LocalDecl : public ValueDecl {
public:
    LocalDecl(size_t handle)
        : handle_(handle)
        , address_taken_(false)
    {}

    size_t handle() const { return handle_; }
    bool is_address_taken() const { return address_taken_; }
    bool is_anonymous() const { return symbol() == Symbol(); }
    virtual std::ostream& print(Printer&) const override;
    virtual thorin::Var var(CodeGen&) const override;

protected:
    size_t handle_;
    mutable bool address_taken_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * parameters
 */

class TypeParam : public TypeDecl {
public:
    const ASTTypes& bounds() const { return bounds_; }
    TypeVar type_var(TypeSema&) const;
    virtual std::ostream& print(Printer&) const override;

private:
    virtual void check(NameSema&) const override;
    virtual Type check(TypeSema&) const override;

    ASTTypes bounds_;

    friend class Parser;
    friend class TypeParamList;
};

class SelfParam : public TypeParam {
public:
    SelfParam(const Location& loc) { symbol_ = Symbol("Self"); }
};

class Param : public LocalDecl {
public:
    Param(size_t handle)
        : LocalDecl(handle)
    {}

    friend class Fn;
    friend class FnDecl;
    friend class Parser;
};

class Fn : public TypeParamList {
public:
    virtual FnType fn_type() const = 0;
    const Param* param(size_t i) const { return params_[i]; }
    ArrayRef<const Param*> params() const { return params_; }
    const Expr* body() const { return body_; }
    thorin::Lambda* lambda() const { return lambda_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Enter* frame() const { return frame_; }
    std::ostream& print_params(Printer& p, bool returning) const;
    void fn_check(NameSema&) const;
    void check_body(TypeSema&, FnType) const;
    thorin::Lambda* emit_head(CodeGen&, const char* name) const;
    void emit_body(CodeGen&) const;

protected:
    mutable thorin::Lambda* lambda_;
    mutable const thorin::Param* ret_param_;
    mutable const thorin::Enter* frame_;

private:
    AutoVector<const Param*> params_;
    AutoPtr<const Expr> body_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * items
 */

class ModContents : public ASTNode {
public:
    const AutoVector<const Item*>& items() const { return items_; }
    virtual std::ostream& print(Printer&) const override;
    void check(NameSema&) const;
    void check(TypeSema&) const;
    void emit(CodeGen&) const;

private:
    AutoVector<const Item*> items_;

    friend class Parser;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }

private:
    virtual void emit(CodeGen&) const = 0;
    Visibility visibility_;

    friend class CodeGen;
    friend class Parser;
};

class TypeDeclItem : public Item, public TypeDecl, public TypeParamList {
    friend class Parser;
};

class ValueItem : public Item, public ValueDecl {
    friend class Parser;
};

class MiscItem : public Item {
private:
    virtual void check(NameSema&) const = 0;
    virtual void check(TypeSema&) const = 0;

    friend class NameSema;
    friend class TypeSema;
};

class ModDecl : public TypeDeclItem {
public:
    const ModContents* mod_contents() const { return mod_contents_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    AutoPtr<const ModContents> mod_contents_;

    friend class Parser;
};

class ForeignMod : public TypeDeclItem {
public:
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;
};

class Typedef : public TypeDeclItem {
public:
    const ASTType* type() const { return type_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    AutoPtr<const ASTType> type_;

    friend class Parser;
};

class FieldDecl : public ValueDecl {
public:
    Visibility visibility() const { return  visibility_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const;

private:
    virtual Type check(TypeSema&) const override;

    Visibility visibility_;

    friend class Parser;
};

class StructDecl : public TypeDeclItem {
public:
    const AutoVector<const FieldDecl*>& fields() const { return fields_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    AutoVector<const FieldDecl*> fields_;

    friend class Parser;
};

class EnumDecl : public TypeDeclItem {
public:
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;
};

class StaticItem : public ValueItem {
public:
    bool is_mut() const { return is_mut_; }
    Symbol symbol() const { return symbol_; }
    const ASTType* type() const { return type_; }
    const Expr* init() const { return init_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    bool is_mut_;
    Symbol symbol_;
    AutoPtr<const ASTType> type_;
    AutoPtr<const Expr> init_;;

    friend class Parser;
};

class FnDecl : public ValueItem, public Fn {
public:
    bool is_extern() const { return extern_; }
    virtual FnType fn_type() const override { return type().as<FnType>(); }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual Type check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    bool extern_;

    friend class Parser;
};

class TraitDecl : public MiscItem, public Decl, public TypeParamList {
public:
    TraitDecl()
        : self_param_(Location(loc().pos1(), loc().pos1()))
    {}

    const AutoVector<const ASTTypeApp*>& super() const { return super_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    const SelfParam* self_param() const { return &self_param_; }
    Trait trait() const { return trait_; }
    Trait to_trait(TypeSema&) const;
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;

private:
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

    const SelfParam self_param_;
    AutoVector<const FnDecl*> methods_;
    AutoVector<const ASTTypeApp*> super_;
    mutable Trait trait_;

    friend class Parser;
};

class Impl : public MiscItem, public TypeParamList {
public:
    /// May be nullptr as trait is optional.
    const ASTType* trait() const { return trait_; }
    const ASTType* for_type() const { return for_type_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    virtual void check(TypeSema&) const override;

    AutoPtr<const ASTType> trait_;
    AutoPtr<const ASTType> for_type_;
    AutoVector<const FnDecl*> methods_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode, public Typeable {
public:
    virtual void check(NameSema&) const = 0;

    void add_inferred_arg(Type t) const { inferred_args_.push_back(t); }
    const thorin::ArrayRef<Type> inferred_args() const { return inferred_args_; }

private:
    virtual std::ostream& print(Printer&) const = 0;
    virtual Type check(TypeSema&, Type) const = 0;
    virtual thorin::Var lemit(CodeGen&) const;
    virtual thorin::Def remit(CodeGen&) const;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const;

    mutable std::vector<Type> inferred_args_;

    friend class CodeGen;
    friend class Parser;
    friend class Printer;
    friend class TypeSema;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const Location& loc) { loc_ = loc; }

    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
    virtual thorin::Def remit(CodeGen&) const override;
};

class BlockExpr : public Expr {
public:
    BlockExpr() {}
    BlockExpr(Location loc) { loc_ = loc; expr_ = new EmptyExpr(loc); }

    const AutoVector<const Stmt*>& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_; }
    const Stmt* stmt(size_t i) const { return stmts_[i]; }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoVector<const Stmt*> stmts_;
    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class LiteralExpr : public Expr {
public:
    enum Kind {
#define IMPALA_LIT(itype, atype) LIT_##itype = Token::LIT_##itype,
#include "impala/tokenlist.h"
        LIT_bool
    };

    LiteralExpr(const Location& loc, Kind kind, thorin::Box box)
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
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    Kind kind_;
    thorin::Box box_;
};

class FnExpr : public Expr, public Fn {
public:
    virtual FnType fn_type() const override { return type().as<FnType>(); }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    bool has_return_type_;

    friend class Parser;
};

class PathExpr : public Expr {
public:
    const Path* path() const { return path_; }
    SafePtr<const ValueDecl> value_decl() const { return value_decl_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
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

    const Expr* rhs() const { return rhs_; }
    Kind kind() const { return kind_; }
    virtual void check(NameSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

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
    virtual void check(NameSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_branch(CodeGen&, thorin::JumpTarget&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

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
    virtual void check(NameSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    Kind kind_;
    AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class FieldExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const PathElem* path_elem() const { return path_elem_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Expr> lhs_;
    AutoPtr<const PathElem> path_elem_;

    friend class Parser;
};

class CastExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const ASTType* ast_type() const { return ast_type(); }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Expr> lhs_;
    AutoPtr<const ASTType> ast_type_;

    friend class Parser;
};

class DefiniteArrayExpr : public Expr {
public:
    const Exprs& elems() const { return elems_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    Exprs elems_;

    friend class Parser;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    const Expr* value() const { return value_; }
    const Expr* count() const { return count_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Expr> value_;
    AutoPtr<const Expr> count_;

    friend class Parser;
};

class IndefiniteArrayExpr : public Expr {
public:
    const Expr* size() const { return size_; }
    const ASTType* elem_type() const { return elem_type_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Expr> size_;
    AutoPtr<const ASTType> elem_type_;

    friend class Parser;
};

class TupleExpr : public Expr {
public:
    const Exprs& elems() const { return elems_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    Exprs elems_;

    friend class Parser;
};

class StructExpr : public Expr {
public:
    class Elem {
    public:
        Elem(Symbol symbol, std::unique_ptr<const Expr> expr)
            : symbol_(symbol)
            , expr_(std::move(expr))
        {}

        Symbol symbol() const { return symbol_; }
        const Expr* expr() const { return expr_.get(); }

    private:
        Symbol symbol_;
        std::unique_ptr<const Expr> expr_;
    };

    typedef std::vector<Elem> Elems;

    const Path* path() const { return path_; }
    const Elems& elems() const { return elems_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Path> path_;
    std::vector<Elem> elems_;

    friend class Parser;
};

class MapExpr : public Expr {
public:
    const Exprs& args() const { return args_; }
    const Expr* arg(size_t i) const { assert(i < args_.size()); return args_[i]; }
    const Expr* lhs() const { return lhs_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const Expr> lhs_;
    Exprs args_;

    friend class Parser;
};

class IfExpr : public Expr {
public:
    const Expr* cond() const { return cond_; }
    const Expr* then_expr() const { return then_expr_; }
    const Expr* else_expr() const { return else_expr_; }
    bool has_else() const;
    virtual void check(NameSema&) const override;
    virtual thorin::Def remit(CodeGen&) const override;
    virtual void emit_jump(CodeGen&, thorin::JumpTarget&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;

    AutoPtr<const Expr> cond_;
    AutoPtr<const Expr> then_expr_;
    AutoPtr<const Expr> else_expr_;

    friend class Parser;
};

class ForExpr : public Expr {
public:
    const FnExpr* fn_expr() const { return fn_expr_; }
    const Expr* expr() const { return expr_; }
    const LocalDecl* break_decl() const { return break_; }
    virtual void check(NameSema&) const override;

private:
    virtual std::ostream& print(Printer&) const override;
    virtual Type check(TypeSema&, Type) const override;
    virtual thorin::Def remit(CodeGen&) const override;

    AutoPtr<const FnExpr> fn_expr_;
    AutoPtr<const Expr> expr_;
    AutoPtr<const LocalDecl> break_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * statements
 */

class Stmt : public ASTNode {
public:
    virtual void check(NameSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual void emit(CodeGen&) const = 0;
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }
    virtual std::ostream& print(Printer&) const override;
    virtual void check(NameSema&) const override;
    virtual void check(TypeSema&) const override;
    virtual void emit(CodeGen&) const override;

private:
    AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    virtual std::ostream& print(Printer&) const override;
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }
    virtual void check(NameSema&) const override;
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
