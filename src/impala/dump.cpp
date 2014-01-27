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
    } else if (auto owned_ptr = type->isa<OwnedPtr>()) {
        stream() << '~';
        return print_type(owned_ptr->referenced_type());
    } else if (auto borrowed_ptr = type->isa<BorrowedPtr>()) {
        stream() << '&';
        return print_type(borrowed_ptr->referenced_type());
    } else if (auto array = type->isa<DefiniteArray>()) {
        stream() << '[';
        return print_type(array->elem_type()) << " * " << array->dim() << ']';
    } else if (auto array = type->isa<IndefiniteArray>()) {
        stream() << '[';
        return print_type(array->elem_type()) << ']';
    } else if (auto tuple = type->isa<TupleType>()) {
        return dump_list([&] (const Type* elem) { print_type(elem); }, tuple->elems(), "(", ")");
    } else if (auto fn = type->isa<FnType>()) {
        const Type* ret_type = fn->return_type();
        if (ret_type->isa<NoRet>())
            return dump_list([&](const Type* elem) { print_type(elem); }, fn->elems(), "fn(", ")");
        else {
            dump_list([&](const Type* elem) { print_type(elem); }, fn->elems().slice_to_end(fn->size()-1), "fn(", ") -> ");
            return print_type(ret_type);
        }
    } else if (auto type_app = type->isa<TypeApp>()) {
        stream() << type_app->name;
        if (!type_app->empty())
            dump_list([&] (const Type* elem) { print_type(elem); }, type_app->elems(), "[", "]");
        return stream();
    } else if (auto primtype = type->isa<PrimType>()) {
        if (primtype->kind() == Token::TYPE_int32)
            return stream() << "int";
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
 * parameters
 */

std::ostream& TypeParam::print(Printer& p) const {
    p.stream() << symbol() << (bounds_.empty() ? "" : ": ");
    return p.dump_list([&] (const Type* type) { p.print_type(type); }, bounds(), "", "", " + ");
}

std::ostream& ParametricType::print_type_params(Printer& p) const {
    if (!type_params().empty())
        p.dump_list([&] (const TypeParam* type_param) { type_param->print(p); }, type_params(), "[", "]");
    return p.stream();
}

/*
 * other decls
 */

std::ostream& Fn::print_params(Printer& p, bool returning) const {
    return p.dump_list([&] (const Param* param) { param->print(p); }, 
            returning ? params().slice_num_from_end(1) : params());
}

std::ostream& LocalDecl::print(Printer& p) const {
    p.stream() << (is_mut() ? "mut " : "" );;
    if (!is_anonymous())
         p.stream() << symbol();
    if (!is_anonymous() && type())
        p.stream() << ": "; 
    if (type())
        p.print_type(type());

    return p.stream();
}

/*
 * items
 */

std::ostream& ModDecl::print(Printer& p) const {
    p.stream() << "mod " << symbol();
    print_type_params(p);
    if (mod_contents()) {
        p.stream() << " {";
        p.up();
        mod_contents()->print(p);
        return p.down() << " }";
    } else
        return p.stream() << ';';
}

std::ostream& FnDecl::print(Printer& p) const {
    p.stream() << "fn " << symbol();
    print_type_params(p);

    const Type* ret = nullptr;
    if (!fn().params().empty() && fn().params().back()->symbol() == "return")
        if (auto fntype = fn().params().back()->type()->isa<FnType>())
            ret = fntype->unpack_return_type();

    p.stream() << '(';
    fn().print_params(p, ret != nullptr);
    p.stream() << ")";

    if (ret) {
        p.stream() << " -> ";
        p.print_type(ret);
    }

    if (auto body = fn().body()) {
        p.stream() << ' ';
        body->print(p);
    } else
        p.stream() << ';';

    return p.stream();
}

std::ostream& FieldDecl::print(Printer& p) const {
    p.stream() << (is_mut() ? "mut " : "" ) << visibility().str() << symbol() << ": ";
    return p.print_type(type());
}

std::ostream& StructDecl::print(Printer& p) const {
    p.stream() << visibility().str() << "struct " << symbol();
    print_type_params(p) << " {";
    p.up();
    p.dump_list([&] (const FieldDecl* field) { field->print(p); }, fields(), "", "", ",", true);
    p.down() << "}";
    return p.stream();
}

std::ostream& TraitDecl::print(Printer& p) const {
    p.stream() << "trait " << symbol();
    print_type_params(p);

    if (!super().empty()) {
        p.stream() << " : ";
        p.dump_list([&] (const Symbol symbol) { p.stream() << symbol; }, super());
    }
        
    p.stream() << " {";
    p.up();
    p.dump_list([&] (const FnDecl* method) { method->print(p); }, methods(), "", "", "", true);
    return p.down() << "}";
}

std::ostream& Impl::print(Printer& p) const {
    p.stream() << "impl " << symbol();
    print_type_params(p);
    if (for_type_) {
        p.stream() << " for ";
        p.print_type(for_type_);
    }
    p.stream() << " {";
    p.up();
    p.dump_list([&] (const FnDecl* method) { method->print(p); }, methods(), "", "", "", true);
    return p.down() << "}";
}

/*
 * item helpers
 */

std::ostream& ModContents::print(Printer& p) const {
    return p.dump_list([&] (const Item* item) { item->print(p); p.newline(); }, items(), "", "", "", true);
}

/*
 * expr
 */

std::ostream& BlockExpr::print(Printer& p) const {
    p.stream() << '{';
    if (empty())
        return p.newline() << '}';
    p.up();
    p.dump_list([&] (const Stmt* stmt) { stmt->print(p); }, stmts(), "", "", "", true);
    if (!expr()->isa<EmptyExpr>()) { 
        if (!stmts().empty())
            p.newline();
        expr()->print(p);
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

std::ostream& FieldExpr::print(Printer& p) const { return lhs()->print(p) << '.' << symbol(); }

std::ostream& StructExpr::print(Printer& p) const { 
    p.stream() << symbol() << '{';
    p.dump_list([&] (const Elem& elem) { p.stream() << elem.symbol() << ": "; elem.expr()->print(p); }, elems());

    return p.stream() << '}';
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
        p.print_type(fn().params().back()->type()->as<FnType>()->unpack_return_type()) << ' ';
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
    p.dump_list([&](const Param* param) { param->print(p); }, fn().params()) << " in ";
    expr()->print(p) << ' ';
    return fn().body()->print(p);
}

/*
 * stmt
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
