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
        else {
            dump_list([&](const Type* elem) { print_type(elem); }, fn->elems().slice_to_end(fn->size()-1), "fn(", ") -> ");
            return print_type(ret_type);
        }
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

std::ostream& LocalDecl::print(Printer& p) const {
    p.stream() << (is_mut() ? "mut " : "" ) << symbol();
    if (auto type = orig_type()) {
        p.stream() << ": "; 
        p.print_type(type);
    }
    return p.stream();
}

std::ostream& Fn::print_params(Printer& p, bool returning) const {
    return p.dump_list([&] (const ParamDecl* param) { param->print(p); }, 
            returning ? params().slice_num_from_end(1) : params());
}

/*
 * Items
 */

std::ostream& ModDecl::print(Printer& p) const {
    return p.stream() << symbol();
}

std::ostream& ModContents::print(Printer& p) const {
    for (auto item : items())
        item->print(p);
    return p.stream();
}

std::ostream& FnDecl::print(Printer& p) const {
    p.stream() << "fn " << symbol();
    //if (!fun()->generics().empty())
        //p.dump_list([&] (const GenericDecl* generic_decl) { generic_decl->print(p); }, fun()->generics(), "<", ">");

    const Type* ret = nullptr;
    if (!fn().params().empty() && fn().params().back()->symbol() == "return")
        if (auto fntype = fn().params().back()->orig_type()->isa<FnType>())
            ret = fntype->unpack_return_type();

    p.stream() << '(';
    fn().print_params(p, ret != nullptr);
    p.stream() << ") ";

    if (ret) {
        p.stream() << "-> ";
        p.print_type(ret) << ' ';
    }

    fn().body()->print(p);
    p.newline();
    return p.newline();
}

std::ostream& StructDecl::print(Printer& p) const {
    assert( false && "todo" );
    return p.stream();
}

/*
 * Expr
 */

std::ostream& BlockExpr::print(Printer& p) const {
    p.stream() << '{';
    p.up();
    p.dump_list([&] (const Stmt* stmt) { stmt->print(p); }, stmts(), "", "", "\n");
    if (!expr()->isa<EmptyExpr>()) {
        p.newline();
        expr()->print(p);
    }

    p.down() << "}";
    return p.stream();
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

std::ostream& FnExpr::print(Printer& p) const { 
    p.stream() << '|';
    fn().print_params(p, has_return_type_);
    p.stream() << "| ";

    if (has_return_type_) {
        p.stream() << "-> ";
        p.print_type(fn().params().back()->orig_type()->as<FnType>()->unpack_return_type()) << ' ';
    }

    return fn().body()->print(p);
}

std::ostream& IfExpr::print(Printer& p) const {
    p.stream() << "if ";
    cond()->print(p) << " ";
    then_expr()->print(p);
    if (has_else()) {
        p.stream() << " else ";
        else_expr()->print(p);
    }
    return p.stream();
}

std::ostream& ForExpr::print(Printer& p) const {
    p.stream() << "for ";
    p.dump_list([&](const ParamDecl* param) { param->print(p); }, fn().params());
    expr()->print(p) << ' ';
    return fn().body()->print(p);
}

/*
 * Stmt
 */

std::ostream& ItemStmt::print(Printer& p) const { return item()->print(p); }

std::ostream& LetStmt::print(Printer& p) const {
    p.stream() << "let ";
    local()->print(p);
    if (init()) {
        p.stream() << " = ";
        init()->print(p);
    }
    return p.stream() << ';';
}

std::ostream& ExprStmt::print(Printer& p) const { 
    bool no_semi = expr()->isa<IfExpr>() || expr()->isa<ForExpr>();
    expr()->print(p); 
    if (!no_semi)
        p.stream() << ';';
    return p.stream();
}

//------------------------------------------------------------------------------

void dump(const ASTNode* n, bool fancy, std::ostream& o) { Printer p(o, fancy); n->print(p); }
std::ostream& operator << (std::ostream& o, const ASTNode* n) { Printer p(o, true); return n->print(p); }
std::ostream& operator << (std::ostream& o, const Type* type) { return Printer(o, true).print_type(type); }

//------------------------------------------------------------------------------

} // namespace impala
