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

namespace thorin {
    class Enter;
    class JumpTarget;
    class Lambda;
}

namespace impala {

class CodeGen;
class Expr;
class FieldDecl;
class FnDecl;
class Item;
class Param;
class TypeParam;
class Printer;
class Sema;
class Stmt;
class Type;
class TypeNode;

typedef thorin::AutoVector<const Type*> Types;
typedef thorin::AutoVector<const Expr*> Exprs;
typedef thorin::AutoVector<const FieldDecl*> Fields;
typedef thorin::AutoVector<const Item*> Items;
typedef thorin::AutoVector<const Param*> Params;
typedef thorin::AutoVector<const TypeParam*> TypeParams;
typedef thorin::AutoVector<const Stmt*> Stmts;
typedef thorin::AutoVector<const FnDecl*> Methods;

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

class ParametricType {
public:
    const TypeParam* type_param(size_t i) const { return type_params_[i]; }
    thorin::ArrayRef<const TypeParam*> type_params() const { return type_params_; }
    std::ostream& print_type_params(Printer&) const;

protected:
    TypeParams type_params_;
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
    const Types& types() const { return types_; }
    virtual std::ostream& print(Printer&) const;

private:
    Symbol symbol_;
    Types types_;

    friend class Parser;
};

typedef thorin::AutoVector<const PathItem*> PathItems;

class Path : public ASTNode {
public:
    bool is_global() const { return is_global_; }
    const PathItems& path_items() const { return path_items_; }
    virtual std::ostream& print(Printer&) const;

private:
    bool is_global_;
    PathItems path_items_;

    friend class Parser;
};

/*
 * types
 */

class Type : public ASTNode {};

class InferType : public Type {
public:
    virtual std::ostream& print(Printer&) const;
};

class ErrorType : public Type {
public:
    virtual std::ostream& print(Printer&) const;
};

class PrimType : public Type {
public:
    enum Kind {
#define IMPALA_TYPE(itype, atype) TYPE_##itype = Token::TYPE_##itype,
#include "impala/tokenlist.h"
    };

    Kind kind() const { return kind_; }
    virtual std::ostream& print(Printer&) const;

private:
    Kind kind_;

    friend class Parser;
};

class PtrType : public Type {
public:
    char kind() const { assert(is_owned() || is_borrowed()); return kind_; }
    const Type* referenced_type() const { return referenced_type_; }
    bool is_owned() const { return kind_ == '~'; }
    bool is_borrowed() const { return kind_ == '&'; }
    virtual std::ostream& print(Printer&) const;

private:
    char kind_;
    thorin::AutoPtr<const Type> referenced_type_;

    friend class Parser;
};

class ArrayType : public Type {
public:
    const Type* elem_type() const { return elem_type_; }

protected:
    thorin::AutoPtr<const Type> elem_type_;

    friend class Parser;
};

class IndefiniteArrayType : public ArrayType {
public:
    virtual std::ostream& print(Printer&) const;
};

class DefiniteArrayType : public ArrayType {
public:
    uint64_t dim() const { return dim_; }
    virtual std::ostream& print(Printer&) const;

private:
    thorin::u64 dim_;

    friend class Parser;
};

class CompoundType : public Type {
public:
    thorin::ArrayRef<const Type*> elems() const { return elems_; }
    const Type* elem(size_t i) const { return elems_[i]; }

protected:
    Types elems_;

    friend class Parser;
};

class TupleType : public CompoundType {
public:
    virtual std::ostream& print(Printer&) const;
};

class TypeApp : public CompoundType {
public:
    Symbol symbol() const { return symbol_; }
    virtual std::ostream& print(Printer&) const;

private:
    Symbol symbol_;

    friend class Parser;
};

class FnType : public ParametricType, public CompoundType {
public:
    const FnType* ret_fn_type() const;
    virtual std::ostream& print(Printer&) const;

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

    friend class ScopeTable;
};

/// Base class for all \p Type declarations.
class TypeDecl : public Decl {
};

/// Base class for all \p Type declarations having \p TypeParams.
class ParametricTypeDecl : public ParametricType, public Decl {
    friend class Parser;
};

/// Base class for all declarations which have a type.
class ValueDecl : public Decl {
public:
    ValueDecl()
        : type_(nullptr)
        , is_mut_(false)
    {}

    /// original type.
    const Type* type() const { return type_; }
    bool is_mut() const { return is_mut_; }

protected:
    thorin::AutoPtr<const Type> type_;
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
    virtual std::ostream& print(Printer&) const;
    bool is_anonymous() const { return symbol() == Symbol(); }

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
    const Types& bounds() const { return bounds_; }
    virtual std::ostream& print(Printer&) const;

private:
    Types bounds_; 

    friend class Parser;
};

class Param : public LocalDecl {
public:
    Param(size_t handle)
        : LocalDecl(handle)
    {}

    friend class Parser;
};

class Fn {
public:
    const Param* param(size_t i) const { return params_[i]; }
    thorin::ArrayRef<const Param*> params() const { return params_; }
    const Expr* body() const { return body_; }
    thorin::Lambda* lambda() const { return lambda_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Enter* frame() const { return frame_; }
    std::ostream& print_params(Printer& p, bool returning) const;
    //void emit(CodeGen& cg) const;

private:
    Params params_;
    thorin::AutoPtr<const Expr> body_;
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
    const Item* item(size_t i) const { return items_[i]; }
    const Items& items() const { return items_; }
    virtual std::ostream& print(Printer&) const;
    void check(Sema& sema) const;

private:
    Items items_;

    friend class Parser;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }
    virtual void check(Sema& sema) const = 0;
    virtual void check_head(Sema& sema) const = 0;
    //virtual void emit(CodeGen& cg) const;

private:
    Visibility visibility_;

    friend class Parser;
};

class ModDecl : public Item, public ParametricTypeDecl {
public:
    const ModContents* mod_contents() const { return mod_contents_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const ModContents> mod_contents_;

    friend class Parser;
};

class ForeignMod : public Item, public ParametricTypeDecl {
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;
};

class Typedef : public Item, public ParametricTypeDecl {
public:
    const Type* type() const { return type_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Type> type_;

    friend class Parser;
};

class FieldDecl : public ValueDecl {
public:
    Visibility visibility() const { return  visibility_; }
    virtual std::ostream& print(Printer&) const;
    void check(Sema& sema) const;
    void check_head(Sema& sema) const;

private:
    Visibility visibility_;

    friend class Parser;
};

class StructDecl : public Item, public ParametricTypeDecl {
public:
    const Fields& fields() const { return fields_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    Fields fields_;

    friend class Parser;
};

class EnumDecl : public Item, public ParametricTypeDecl {
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;
};

class StaticItem : public Item, public ValueDecl {
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;
};

class FnDecl : public ParametricType, public Item, public ValueDecl {
public:
    const Fn& fn() const { return fn_; }
    bool is_extern() const { return extern_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    Fn fn_;
    bool extern_;

    friend class Parser;
};

class TraitDecl : public Item, public ParametricTypeDecl {
public:
    const std::vector<Symbol>& super() const { return super_; }
    const Methods& methods() const { return methods_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    Methods methods_;
    std::vector<Symbol> super_;

    friend class Parser;
};

class Impl : public Item, public ParametricTypeDecl {
public:
    const Type* for_type() const { return for_type_; }
    const Methods& methods() const { return methods_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    virtual void check_head(Sema& sema) const;
    //virtual void emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Type> for_type_;
    Methods methods_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * expressions
 */

class Expr : public ASTNode {
public:
    const TypeNode* type() const { return type_; }
    virtual bool is_lvalue() const = 0;
    virtual void check(Sema& sema) const = 0;
    virtual thorin::RefPtr emit(CodeGen& cg) const { /*= 0*/ return 0; }
    virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const {}

private:
    mutable const TypeNode* type_;
    friend class Parser;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const Location& loc) { loc_ = loc; }

    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
};

class BlockExpr : public Expr {
public:
    BlockExpr() {}
    BlockExpr(Location loc) { loc_ = loc; expr_ = new EmptyExpr(loc); }

    const Stmts& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_; }
    const Stmt* stmt(size_t i) const { return stmts_[i]; }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Stmts stmts_;
    thorin::AutoPtr<const Expr> expr_;

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
    TokenKind literal2type() const;
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;
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
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:

    Fn fn_;
    bool has_return_type_;

    friend class Parser;
};

class PathExpr : public Expr {
public:
    PathExpr() 
        : decl_(nullptr)
    {}

    const Path* path() const { return path_; }
    const Decl* decl() const { return decl_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const;
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Path> path_;
    mutable const Decl* decl_; ///< Declaration of the variable in use.

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
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

private:
    Kind kind_;
    thorin::AutoPtr<const Expr> rhs_;

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
    virtual void check(Sema& sema) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

private:
    Kind kind_;
    thorin::AutoPtr<const Expr> lhs_;
    thorin::AutoPtr<const Expr> rhs_;

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
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    Kind kind_;
    thorin::AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class FieldExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    Symbol symbol() const { return symbol_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return true; }
    virtual void check(Sema& sema) const;

private:
    thorin::AutoPtr<const Expr> lhs_;
    Symbol symbol_;

    friend class Parser;
};

class CastExpr : public Expr {
public:
    const Expr* lhs() const { return lhs_; }
    const Type* as() const { return as_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;

private:
    thorin::AutoPtr<const Expr> lhs_;
    thorin::AutoPtr<const Type> as_;

    friend class Parser;
};

class OpsExpr : public Expr {
public:
    const Exprs& ops() const { return ops_; }
    const Expr* op(size_t i) const { return ops_[i]; }
    virtual bool is_lvalue() const { return false; }

protected:
    Exprs ops_;
};

class DefiniteArrayExpr : public OpsExpr {
public:
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    friend class Parser;
};

class RepeatedDefiniteArrayExpr : public Expr {
public:
    const Expr* value() const { return value_; }
    const Expr* count() const { return count_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Expr> value_;
    thorin::AutoPtr<const Expr> count_;

    friend class Parser;
};

class IndefiniteArrayExpr : public Expr {
public:
    const Expr* size() const { return size_; }
    const Type* elem_type() const { return elem_type_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Expr> size_;
    thorin::AutoPtr<const Type> elem_type_;

    friend class Parser;
};

class TupleExpr : public OpsExpr {
public:
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

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
    virtual void check(Sema& sema) const;

private:
    thorin::AutoPtr<const Path> path_;
    std::vector<Elem> elems_;

    friend class Parser;
};

class MapExpr : public OpsExpr {
public:
    const Expr* lhs() const { return lhs_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const;
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Expr> lhs_;

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
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Expr> cond_;
    thorin::AutoPtr<const Expr> then_expr_;
    thorin::AutoPtr<const Expr> else_expr_;

    friend class Parser;
};

class ForExpr : public Expr {
public:
    const Expr* expr() const { return expr_; }
    const Fn& fn() const { return fn_; }
    virtual std::ostream& print(Printer&) const;
    virtual bool is_lvalue() const { return false; }
    virtual void check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

private:
    thorin::AutoPtr<const Expr> expr_;
    Fn fn_;

    friend class Parser;
};

//------------------------------------------------------------------------------

/*
 * statements
 */

class Stmt : public ASTNode {
public:
    virtual void check(Sema& sema) const = 0;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const { [>= 0<]; }
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    thorin::AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }
    virtual std::ostream& print(Printer&) const;
    virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    thorin::AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    virtual std::ostream& print(Printer&) const;
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }
    virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

private:
    thorin::AutoPtr<const LocalDecl> local_;
    thorin::AutoPtr<const Expr> init_;

    friend class Parser;
};

//------------------------------------------------------------------------------

}

#endif
