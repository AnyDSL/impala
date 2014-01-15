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

#include "impala/token.h"
#include "impala/type.h"

namespace thorin {
    class DefNode;
    class Enter;
    class JumpTarget;
    class Lambda;
    class ParamDecl;
    class Ref;
}

namespace impala {

class BlockExpr;
class CodeGen;
class Expr;
class Fn;
class Item;
class ParamDecl;
class Printer;
class Sema;
class Stmt;
//class GenericDecl;

typedef thorin::AutoVector<const Expr*> Exprs;
typedef thorin::AutoVector<const Item*> Items;
typedef thorin::AutoVector<const ParamDecl*> Params;
typedef thorin::AutoVector<const Stmt*> Stmts;
//typedef thorin::AutoVector<const GenericDecl*> GenericDecls;

//------------------------------------------------------------------------------

enum class Visibility {
    None, 
    Pub = Token::PUB, 
    Priv = Token::PRIV
};

class ASTNode : public thorin::HasLocation, public thorin::MagicCast<ASTNode> {
public:
#ifndef NDEBUG
    virtual ~ASTNode() { assert(loc_.is_set()); }
#endif
    virtual std::ostream& print(Printer& p) const = 0;
    void dump() const;
};

//------------------------------------------------------------------------------

class Decl : virtual public ASTNode {
public:
    thorin::Symbol symbol() const { return symbol_; }
    size_t depth() const { return depth_; }
    const Decl* shadows() const { return shadows_; }

protected:
    thorin::Symbol symbol_;

private:
    mutable const Decl* shadows_;
    mutable size_t depth_;

    friend class Sema;
};

/// Base class for all module declarations.
class PathDecl : public Decl {
};

/// Base class for all \p Type declarations.
class TypeDecl : public Decl {
};

/// Base class for all declarations which have a type.
class ValueDecl : public Decl {
public:
    ValueDecl()
        : orig_type_(nullptr)
        , refined_type_(nullptr)
        , is_mut_(false)
    {}

    const Type* orig_type() const { return orig_type_; }
    const Type* refined_type() const { return refined_type_; }
    bool is_mut() const { return is_mut_; }

protected:
    const Type* orig_type_;
    mutable const Type* refined_type_;
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
    virtual std::ostream& print(Printer& p) const;

protected:
    size_t handle_;
    mutable bool is_address_taken_;

    friend class Parser;
};

//------------------------------------------------------------------------------

class ParamDecl : public LocalDecl {
public:
    ParamDecl(size_t handle)
        : LocalDecl(handle)
    {}

    const Fn* fn() const { return fn_; }

private:
    mutable const Fn* fn_;

    friend class Parser;
};

class Fn {
public:
    const ParamDecl* param(size_t i) const { return params_[i]; }
    const Params& params() const { return params_; }
    const Expr* body() const { return body_; }
    thorin::Lambda* lambda() const { return lambda_; }
    const thorin::Param* ret_param() const { return ret_param_; }
    const thorin::Enter* frame() const { return frame_; }
    std::ostream& print_params(Printer& p) const;
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

class ModContents : public ASTNode {
public:
    const Item* item(size_t i) const { return items_[i]; }
    const Items& items() const { return items_; }
    virtual std::ostream& print(Printer& p) const;

private:
    Items items_;

    friend class Parser;
};

class Item : virtual public ASTNode {
public:
    Visibility visibility() const { return  visibility_; }

private:
    Visibility visibility_;

    friend class Parser;
};

class ModDecl : public Item, public PathDecl {
public:
    const ModContents* mod_contents() const { return mod_contents_; }
    virtual std::ostream& print(Printer& p) const;

private:
    thorin::AutoPtr<const ModContents> mod_contents_;

    friend class Parser;
};

class ForeignMod : public Item, public PathDecl {
    virtual std::ostream& print(Printer& p) const;
};

class Typedef : public Item, public TypeDecl {
    virtual std::ostream& print(Printer& p) const;
};

class StructDecl : public Item, public TypeDecl {
    virtual std::ostream& print(Printer& p) const;
};

class EnumDecl : public Item, public TypeDecl {
    virtual std::ostream& print(Printer& p) const;
};

class TraitDecl : public Item, public TypeDecl {
    virtual std::ostream& print(Printer& p) const;
};

class ConstItem : public Item {
    virtual std::ostream& print(Printer& p) const;
};

class Impl : public Item {
    virtual std::ostream& print(Printer& p) const;
};

class FnDecl : public Item, public ValueDecl {
public:
    FnDecl(TypeTable& typetable);

    const Fn& fn() const { return fn_; }
    bool is_extern() const { return extern_; }
    void check_head(Sema&) const;
    //virtual void check(Sema& sema) const;
    virtual std::ostream& print(Printer& p) const;
    //virtual void emit(CodeGen& cg) const;

private:
    Fn fn_;
    bool extern_;

    friend class Parser;
};

//------------------------------------------------------------------------------

class Expr : public ASTNode {
public:
    Expr() 
        : type_(nullptr) 
    {}

    const Type* type() const { return type_; }
    virtual bool is_lvalue() const = 0;

private:
    virtual thorin::RefPtr emit(CodeGen& cg) const { /*= 0*/ return 0; }
    virtual const Type* check(Sema& sema) const { /*= 0*/ return 0; }

protected:
    virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const {}

    mutable const Type* type_;

    friend class Parser;
    friend class Sema;
    friend class CodeGen;
};

class EmptyExpr : public Expr {
public:
    EmptyExpr(const thorin::Location& loc) { loc_ = loc; }

    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
};

class BlockExpr : public Expr {
public:
    BlockExpr() {}
    BlockExpr(thorin::Location loc) { loc_ = loc; }

    const Stmts& stmts() const { return stmts_; }
    const Expr* expr() const { return expr_; }
    const Stmt* stmt(size_t i) const { return stmts_[i]; }
    bool empty() const { return stmts_.empty() && expr_->isa<EmptyExpr>(); }
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
    //virtual const Type* check(Sema& sema) const;

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

    LiteralExpr(const thorin::Location& loc, Kind kind, thorin::Box box)
        : kind_(kind)
        , box_(box)
    {
        loc_= loc;
    }

    Kind kind() const { return kind_; }
    thorin::Box box() const { return box_; }
    uint64_t get_u64() const;
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;
    TokenKind literal2type() const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    Kind kind_;
    thorin::Box box_;
};

class FnExpr : public Expr {
public:
    FnExpr(TypeTable& typetable)
        //: fn_(new Fn(typetable))
    {}

    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;
    const Fn& fn() const { return fn_; }

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    Fn fn_;

    friend class Parser;
};

class IdExpr : public Expr {
public:
    IdExpr(const Token& tok)
        : symbol_(tok.symbol())
        , decl_(nullptr)
    {
        loc_ = tok.loc();
    }

    thorin::Symbol symbol() const { return symbol_; }
    const Decl* decl() const { return decl_; }

    virtual bool is_lvalue() const;
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    thorin::Symbol symbol_;
    mutable const Decl* decl_; ///< Declaration of the variable in use.
};

class PrefixExpr : public Expr {
public:
    enum Kind {
#define IMPALA_PREFIX(tok, str, prec) tok = Token:: tok,
#include "impala/tokenlist.h"
    };

    const Expr* rhs() const { return rhs_;; }
    Kind kind() const { return kind_; }
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

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
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;
    //virtual void emit_branch(CodeGen& cg, thorin::JumpTarget& t, thorin::JumpTarget& f) const;

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
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    Kind kind_;
    thorin::AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class OpsExpr : public Expr {
public:
    const Exprs& ops() const { return ops_; }
    const Expr* op(size_t i) const { return ops_[i]; }

protected:
    Exprs ops_;
};

class ArrayExpr : public OpsExpr {
public:
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    friend class Parser;
};

class TupleExpr : public OpsExpr {
public:
    virtual bool is_lvalue() const { return false; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    friend class Parser;
};

class MapExpr : public OpsExpr {
public:
    const Expr* lhs() const { return lhs_; }
    virtual bool is_lvalue() const;
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    thorin::AutoPtr<const Expr> lhs_;

    friend class Parser;
};

class IfExpr : public Expr {
public:
    const Expr* cond() const { return cond_; }
    const BlockExpr* then_block() const { return then_block_; }
    const BlockExpr* else_block() const { return else_block_; }
    virtual std::ostream& print(Printer& p) const;
    virtual bool is_lvalue() const { return false; }

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    thorin::AutoPtr<const Expr> cond_;
    thorin::AutoPtr<const BlockExpr> then_block_;
    thorin::AutoPtr<const BlockExpr> else_block_;

    friend class Parser;
};

class ForExpr : public Expr {
public:
    const Expr* expr() const { return expr_; }
    const Fn& fn() const { return fn_; }
    virtual std::ostream& print(Printer& p) const;
    virtual bool is_lvalue() const { return false; }

private:
    //virtual const Type* check(Sema& sema) const;
    //virtual thorin::RefPtr emit(CodeGen& cg) const;

    thorin::AutoPtr<const Expr> expr_;
    Fn fn_;

    friend class Parser;
};

//------------------------------------------------------------------------------

class Stmt : public ASTNode {
private:
    //virtual void check(Sema& sema) const { [>= 0<]; }
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const { [>= 0<]; }

    friend class Sema;
    friend class CodeGen;
};

class ExprStmt : public Stmt {
public:
    const Expr* expr() const { return expr_; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

    thorin::AutoPtr<const Expr> expr_;

    friend class Parser;
};

class ItemStmt : public Stmt {
public:
    const Item* item() const { return item_; }
    virtual std::ostream& print(Printer& p) const;

private:
    //virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

    thorin::AutoPtr<const Item> item_;

    friend class Parser;
};

class LetStmt : public Stmt {
public:
    virtual std::ostream& print(Printer& p) const;
    const LocalDecl* local() const { return local_; }
    const Expr* init() const { return init_; }

private:
    //virtual void check(Sema& sema) const;
    //virtual void emit(CodeGen& cg, thorin::JumpTarget& exit) const;

    thorin::AutoPtr<const LocalDecl> local_;
    thorin::AutoPtr<const Expr> init_;

    friend class Parser;
};

//------------------------------------------------------------------------------

} // namespace impala

#endif // IMPALA_AST_H
