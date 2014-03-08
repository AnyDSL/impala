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
class CodeGen;
class Decl;
class Expr;
class FieldDecl;
class FnDecl;
class Item;
class NameSema;
class Param;
class Printer;
class Stmt;
class TypeDecl;
class TypeOrTraitDecl;
class TypeParam;
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

class PathItem : public ASTNode {
public:
    Symbol symbol() const { return symbol_; }
    const ASTTypes& args() const { return args_; }
    const Decl* decl() const { return decl_; }
    virtual std::ostream& print(Printer&) const;
    void check(NameSema&) const;

private:
    Symbol symbol_;
    ASTTypes args_;
    mutable SafePtr<const Decl> decl_;

    friend class Parser;
};

typedef AutoVector<const PathItem*> PathItems;

class Path : public ASTNode {
public:
    bool is_global() const { return is_global_; }
    const PathItems& path_items() const { return path_items_; }
    virtual std::ostream& print(Printer&) const;
    void check(NameSema&) const;
    const Decl* decl() const { return path_items_.back()->decl(); }

private:
    bool is_global_;
    PathItems path_items_;

    friend class Parser;
};

/*
 * types
 */

class ASTType : public ASTNode {
public:
    virtual void check(NameSema&) const = 0;
    virtual Type to_type(TypeSema&) const = 0;
};

class ErrorASTType : public ASTType {
public:
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;
};

class PrimASTType : public ASTType {
public:
    enum Kind {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    Kind kind() const { return kind_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;

private:
    Kind kind_;

    friend class Parser;
};

class PtrASTType : public ASTType {
public:
    char kind() const { assert(is_owned() || is_borrowed()); return kind_; }
    const ASTType* referenced_type() const { return referenced_type_; }
    bool is_owned() const { return kind_ == '~'; }
    bool is_borrowed() const { return kind_ == '&'; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;
};

class DefiniteArrayASTType : public ArrayASTType {
public:
    uint64_t dim() const { return dim_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;
};

class ASTTypeApp : public CompoundASTType {
public:
    Symbol symbol() const { return symbol_; }
    const TypeOrTraitDecl* type_or_trait_decl() const { return type_or_trait_decl_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;
    Trait to_trait(TypeSema&) const;

private:
    Symbol symbol_;
    mutable SafePtr<const TypeOrTraitDecl> type_or_trait_decl_;

    friend class Parser;
    friend class NameScope;
};

class FnASTType : public TypeParamList, public CompoundASTType {
public:
    const FnASTType* ret_fn_type() const;
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual Type to_type(TypeSema&) const;

    friend class Parser;
};

/// Base class of all items that have a type assigned. Use as a mixin
class Typable {
public:
    Type type() const { return type_; }
protected:
    Typable() : type_() {}
    void set_type(Type t) const;
private:
    mutable Type type_;
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

class TypeOrTraitDecl : public Decl {
};

/// Base class for all \p Type declarations.
class TypeDecl : public TypeOrTraitDecl {
public:
    virtual Type to_type(TypeSema&) const = 0;
};

/// Base class for all \p Type declarations having \p TypeParam%s.
class ParametricTypeDecl : public TypeDecl, public TypeParamList {
    friend class Parser;
};

/// Base class for all declarations which have a type.
class ValueDecl : public Decl, public Typable {
public:
    ValueDecl()
        : is_mut_(false)
    {}

    const ASTType* ast_type() const { return ast_type_; } ///< Original \p ASTType.
    bool is_mut() const { return is_mut_; }
    void check(NameSema&) const;
    void check(TypeSema&) const;
    Type calc_type(TypeSema&) const;

protected:
    AutoPtr<const ASTType> ast_type_;
    bool is_mut_;

    friend class Parser;
};

/// Base class for all Values which may be mutated within a function.
class LocalDecl : public ValueDecl {
public:
    LocalDecl(size_t handle)
        : handle_(handle)
    {}

    size_t handle() const { return handle_; }
    bool is_anonymous() const { return symbol() == Symbol(); }
    virtual std::ostream& print(Printer&) const;

protected:
    size_t handle_;
    mutable bool is_address_taken_;

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
    virtual std::ostream& print(Printer&) const;
    virtual Type to_type(TypeSema&) const;

private:
    ASTTypes bounds_;
    mutable TypeVar type_var_;

    friend class Parser;
    friend class TypeParamList;
};

class Param : public LocalDecl {
public:
    Param(size_t handle)
        : LocalDecl(handle)
    {}

    friend class FnDecl;
    friend class Parser;
};

class Fn {
public:
    const Param* param(size_t i) const { return params_[i]; }
    ArrayRef<const Param*> params() const { return params_; }
    const Expr* body() const { return body_; }
    thorin::Lambda* lambda() const { return lambda_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Enter* frame() const { return frame_; }
    std::ostream& print_params(Printer& p, bool returning) const;
    //void emit(CodeGen& cg) const;

private:
    AutoVector<const Param*> params_;
    AutoPtr<const Expr> body_;
    mutable thorin::Lambda* lambda_;
    mutable const thorin::Param* ret_param_;
    mutable const thorin::Enter* frame_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * items
 */

class ModContents : public ASTNode {
public:
    const AutoVector<const Item*>& items() const { return items_; }
    virtual std::ostream& print(Printer&) const;
    void check(NameSema&) const;
    void check(TypeSema&) const;

private:
    AutoVector<const Item*> items_;

    friend class Parser;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }
    virtual void check_head(NameSema&) const = 0;
    virtual void check(NameSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    //virtual void emit(CodeGen& cg) const;

private:
    Visibility visibility_;

    friend class Parser;
};

class ModDecl : public Item, public ParametricTypeDecl {
public:
    const ModContents* mod_contents() const { return mod_contents_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual Type to_type(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    AutoPtr<const ModContents> mod_contents_;

    friend class Parser;
};

class ForeignMod : public Item, public ParametricTypeDecl {
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual Type to_type(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;
};

class Typedef : public Item, public ParametricTypeDecl {
public:
    const ASTType* type() const { return type_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual Type to_type(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    AutoPtr<const ASTType> type_;

    friend class Parser;
};

class FieldDecl : public ValueDecl {
public:
    Visibility visibility() const { return  visibility_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;

private:
    Visibility visibility_;

    friend class Parser;
};

class StructDecl : public Item, public ParametricTypeDecl {
public:
    const AutoVector<const FieldDecl*>& fields() const { return fields_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual Type to_type(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    AutoVector<const FieldDecl*> fields_;

    friend class Parser;
};

class EnumDecl : public Item, public ParametricTypeDecl {
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual Type to_type(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;
};

class StaticItem : public Item, public ValueDecl {
public:
    bool is_mut() const { return is_mut_; }
    Symbol symbol() const { return symbol_; }
    const ASTType* type() const { return type_; }
    const Expr* init() const { return init_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    bool is_mut_;
    Symbol symbol_;
    AutoPtr<const ASTType> type_;
    AutoPtr<const Expr> init_;;

    friend class Parser;
};

class FnDecl : public TypeParamList, public Item, public ValueDecl {
public:
    const Fn& fn() const { return fn_; }
    bool is_extern() const { return extern_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    Fn fn_;
    bool extern_;

    friend class Parser;
};

class TraitDecl : public Item, public TypeOrTraitDecl, public TypeParamList {
public:
    const AutoVector<const ASTTypeApp*>& super() const { return super_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    Trait trait() const { return trait_; }
    Trait to_trait(TypeSema&) const;
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    AutoVector<const FnDecl*> methods_;
    AutoVector<const ASTTypeApp*> super_;
    mutable Trait trait_;

    friend class Parser;
};

class Impl : public Item, public TypeParamList {
public:
    /// May be nullptr as trait is optional.
    const ASTType* trait() const { return trait_; }
    const ASTType* for_type() const { return for_type_; }
    const AutoVector<const FnDecl*>& methods() const { return methods_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check_head(NameSema&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg) const;

private:
    AutoPtr<const ASTType> trait_;
    AutoPtr<const ASTType> for_type_;
    AutoVector<const FnDecl*> methods_;
    mutable bool checked_ = false;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode, public Typable {
public:
    virtual bool is_lvalue() const = 0;
    virtual void check(NameSema&) const = 0;
    virtual void check(TypeSema&) const = 0;
    virtual thorin::RefPtr emit(CodeGen& cg) const { /*= 0*/ return 0; }
    virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const {}

private:
    friend class Parser;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const Location& loc) { loc_ = loc; }

    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;

private:
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
};

class BlockExpr : public Expr {
public:
    BlockExpr() {}
    BlockExpr(Location loc) { loc_ = loc; expr_ = new EmptyExpr(loc); }

    const AutoVector<const Stmt*>& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_; }
    const Stmt* stmt(size_t i) const { return stmts_[i]; }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Kind kind_;
    thorin::Box box_;
};

class FnExpr : public Expr {
public:
    const Fn& fn() const { return fn_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Fn fn_;
    bool has_return_type_;

    friend class Parser;
};

class PathExpr : public Expr {
public:
    const Path* path() const { return path_; }
    const ValueDecl* value_decl() const { return value_decl_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
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

    const Expr* rhs() const { return rhs_;; }
    Kind kind() const { return kind_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

private:
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
    virtual bool is_lvalue() const { return Token::is_assign((TokenKind) kind()); }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Kind kind_;
    AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class FieldExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    Symbol symbol() const { return symbol_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return true; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;

private:
    AutoPtr<const Expr> lhs_;
    Symbol symbol_;

    friend class Parser;
};

class CastExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const ASTType* as() const { return as_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;

private:
    AutoPtr<const Expr> lhs_;
    AutoPtr<const ASTType> as_;

    friend class Parser;
};

class DefiniteArrayExpr : public Expr {
public:
    const Exprs& elems() const { return elems_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual bool is_lvalue() const { return false; }
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Exprs elems_;

    friend class Parser;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    const Expr* value() const { return value_; }
    const Expr* count() const { return count_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    AutoPtr<const Expr> value_;
    AutoPtr<const Expr> count_;

    friend class Parser;
};

class IndefiniteArrayExpr : public Expr {
public:
    const Expr* size() const { return size_; }
    const ASTType* elem_type() const { return elem_type_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    AutoPtr<const Expr> size_;
    AutoPtr<const ASTType> elem_type_;

    friend class Parser;
};

class TupleExpr : public Expr {
public:
    const Exprs& elems() const { return elems_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    virtual bool is_lvalue() const { return false; }
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;

private:
    AutoPtr<const Path> path_;
    std::vector<Elem> elems_;

    friend class Parser;
};

class MapExpr : public Expr {
public:
    const Exprs& args() const { return args_; }
    const Expr* lhs() const { return lhs_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
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
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    AutoPtr<const Expr> cond_;
    AutoPtr<const Expr> then_expr_;
    AutoPtr<const Expr> else_expr_;

    friend class Parser;
};

class ForExpr : public Expr {
public:
    const Expr* expr() const { return expr_; }
    const Fn& fn() const { return fn_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    AutoPtr<const Expr> expr_;
    Fn fn_;

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
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const { [>= 0<]; }
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    virtual std::ostream& print(Printer&) const;
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }
    virtual void check(NameSema&) const;
    virtual void check(TypeSema&) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    AutoPtr<const LocalDecl> local_;
    AutoPtr<const Expr> init_;

    friend class Parser;
};

//------------------------------------------------------------------------------

}

#endif
