#include "impala/dump.h"

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/prec.h"
#include "impala/type.h"

using thorin::ArrayRef;
using thorin::Type;
using thorin::Symbol;

namespace impala {

std::ostream& Printer::print_type(const Type* type) {
    if (type == nullptr) {
        return stream() << "<NULL>";
    } else if (type->isa<NoRet>()) {
        return stream() << "noret";
    } else if (type->isa<Void>()) {
        return stream() << "void";
    } else if (type->isa<TypeError>()) {
        return stream() << "<error>";
    } else if (auto array = type->isa<DefiniteArray>()) {
        return stream() << '[' << array->elem_type() << " * " << array->dim() << ']';
    } else if (auto array = type->isa<IndefiniteArray>()) {
        return stream() << '[' << array->elem_type() << ']';
    } else if (auto tuple = type->isa<TupleType>()) {
        return dump_list([&](const Type* elem) { print_type(elem); }, tuple->elems(), "(", ")");
    } else if (auto fn = type->isa<FnType>()) {
        const Type* ret_type = fn->return_type();
        if (ret_type->isa<NoRet>())
            return dump_list([&](const Type* elem) { print_type(elem); }, fn->elems(), "fn(", ")");
        else
            return dump_list([&](const Type* elem) { print_type(elem); }, fn->elems().slice_to_end(fn->size()-1), "fn(", ") -> ") 
                << ret_type;
    } else if (auto idtype = type->isa<IdType>()) {
        return stream() << idtype->name;
    } else if (auto primtype = type->isa<PrimType>()) {
        switch (primtype->kind()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return stream() << #itype;
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    }
    THORIN_UNREACHABLE;
}

//------------------------------------------------------------------------------

void ASTNode::dump() const { Printer p(std::cout, true); print(p) << std::endl; }
void Type::dump() const { Printer p(std::cout, true); p.print_type(this) << std::endl; }

//------------------------------------------------------------------------------

/*
 * Items
 */

std::ostream& FnDecl::print(Printer& p) const {
    //p.stream() << "fn " << fun()->symbol(); 
    //if (!fun()->generics().empty())
        //p.dump_list([&] (const GenericDecl* generic_decl) { generic_decl->print(p); }, fun()->generics(), "<", ">");

    //return fun()->print(p);
    return p.stream();
}

std::ostream& StructDecl::print(Printer& p) const {
    assert( false && "todo" );
    return p.stream();
}

/*
 * Expr
 */

std::ostream& BlockExpr::print(Printer& p) const {
    p.stream() << "{";
    p.up();
    if (!empty()) {
        for (auto i = stmts().cbegin(), e = stmts().cend() - 1; i != e; ++i) {
            (*i)->print(p);
            p.newline();
        }

        stmts().back()->print(p);
    }
    return p.down() << "}";
}

std::ostream& LiteralExpr::print(Printer& p) const {
    switch (kind()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return p.stream() << box().get_##atype();
#include "impala/tokenlist.h"
        case LIT_bool: return p.stream() << (box().get_bool() ? "true" : "false");
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& IdExpr   ::print(Printer& p) const { return p.stream() << symbol(); }
std::ostream& EmptyExpr::print(Printer& p) const { return p.stream() << "/*empty*/"; }
std::ostream& ArrayExpr::print(Printer& p) const { return p.dump_list([&](const Expr* expr) { expr->print(p); }, ops(), "[", "]"); }
std::ostream& TupleExpr::print(Printer& p) const { return p.dump_list([&](const Expr* expr) { expr->print(p); }, ops(), "(", ")"); }

std::ostream& PrefixExpr::print(Printer& p) const {
    Prec r = PrecTable::prefix_r[kind()];
    Prec old = p.prec;

    const char* op;
    switch (kind()) {
#define IMPALA_PREFIX(tok, str, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }

    p.stream() << op;
    p.prec = r;
    rhs()->print(p);
    p.prec = old;

    return p.stream();
}

std::ostream& InfixExpr::print(Printer& p) const {
    Prec l = PrecTable::infix_l[kind()];
    Prec r = PrecTable::infix_r[kind()];
    Prec old = p.prec;
    bool paren = !p.is_fancy() || p.prec > l;

    if (paren) p.stream() << "(";

    p.prec = l;
    lhs()->print(p);

    const char* op;
    switch (kind()) {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) case tok: op = str; break;
#define IMPALA_INFIX(     tok, str, lprec, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
    }

    p.stream() << " " << op << " ";

    p.prec = r;
    rhs()->print(p);
    p.prec = old;

    if (paren) p.stream() << ")";

    return p.stream();
}

std::ostream& PostfixExpr::print(Printer& p) const {
    Prec l = PrecTable::postfix_l[kind()];
    Prec old = p.prec;
    bool paren = !p.is_fancy() || p.prec > l;

    if (paren) p.stream() << "(";

    p.prec = l;
    lhs()->print(p);

    const char* op;
    switch (kind()) {
        case INC: op = "++"; break;
        case DEC: op = "--"; break;
        default: THORIN_UNREACHABLE;
    }

    p.stream() << op;
    p.prec = old;

    if (paren) p.stream() << ")";

    return p.stream();
}

std::ostream& MapExpr::print(Printer& p) const {
    lhs()->print(p);
    return p.dump_list([&](const Expr* expr) { expr->print(p); }, ops(), "(", ")");
}

std::ostream& FnExpr::print(Printer& p) const { /*...*/ }

std::ostream& IfElseExpr::print(Printer& p) const {
    p.stream() << "if " << cond() << " ";
    then_block()->print(p);

    if (!else_block()->empty()) {
        p.stream() << " else ";
        else_block()->print(p);
    }

    return p.stream();
}

std::ostream& ForExpr::print(Printer& p) const {
    p.stream() << "for ";
    p.dump_list([&](const Param* param) { param->print(p); }, fn().params());
    expr()->print(p) << ' ';
    return fn().body()->print(p);
}

/*
 * Stmt
 */

std::ostream& ItemStmt::print(Printer& p) const { return item()->print(p); }

std::ostream& LetStmt::print(Printer& p) const {
    p.stream() << "let ";
    //var_decl()->print(p);
    //if (init()) {
        //p.stream() << " = ";
        //init()->print(p) << ";";
    //}
    return p.stream();
}

std::ostream& ExprStmt::print(Printer& p) const {
    expr()->print(p);
    return p.stream() << ";";
}

//------------------------------------------------------------------------------

void dump_prg(const Scope* scope, bool fancy, std::ostream& o) { 
    Printer p(o, fancy);
    //for (auto stmt : scope->stmts()) {
        //stmt->print(p);
        //p.newline();
    //}
}

void dump(const ASTNode* n, bool fancy, std::ostream& o) { Printer p(o, fancy); n->print(p); }
std::ostream& operator << (std::ostream& o, const ASTNode* n) { Printer p(o, true); return n->print(p); }
std::ostream& operator << (std::ostream& o, const Type* type) { return Printer(o, true).print_type(type); }

//------------------------------------------------------------------------------

} // namespace impala
