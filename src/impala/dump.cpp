#include "impala/dump.h"

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/prec.h"

using thorin::ArrayRef;
using thorin::Type;

namespace impala {

//------------------------------------------------------------------------------

void ASTNode::dump() const { Printer p(std::cout, true); print(p) << std::endl; }

//------------------------------------------------------------------------------

/*
 * paths
 */

std::ostream& PathItem::print(Printer& p) const { 
    p.stream() << symbol();
    return print_type_params(p);
}

std::ostream& Path::print(Printer& p) const {
    p.stream() << (is_global() ? "::" : "");
    return p.dump_list([&] (const PathItem* path_item) { path_item->print(p); }, path_items(), "", "", "::");
}

/*
 * types
 */

std::ostream& InferType::print(Printer& p) const { return p.stream() << "<infer>"; }
std::ostream& ErrorType::print(Printer& p) const { return p.stream() << "<error>"; }

std::ostream& PtrType::print(Printer& p) const { 
    p.stream() << kind();
    return referenced_type()->print(p);
}

std::ostream& DefiniteArrayType::print(Printer& p) const { 
    p.stream() << '[';
    return elem_type()->print(p) << " * " << dim() << ']';
}

std::ostream& IndefiniteArrayType::print(Printer& p) const { 
    p.stream() << '[';
    return elem_type()->print(p) << ']';
}

std::ostream& TupleType::print(Printer& p) const { 
    return p.dump_list([&] (const Type* elem) { elem->print(p); }, elems(), "(", ")");
}

std::ostream& FnType::print(Printer& p) const { 
    auto ret = ret_fn_type();
    p.stream() << "fn";
    print_type_params(p);
    p.dump_list([&] (const Type* elem) { elem->print(p); }, ret != nullptr ? elems().slice_num_from_end(1) : elems(), "(", ")");
    if (ret != nullptr) {
        p.stream() << " -> ";
        ret->print(p);
    }
    return p.stream();
}

std::ostream& TypeApp::print(Printer& p) const { 
    p.stream() << symbol();
    if (!elems().empty())
        p.dump_list([&] (const Type* elem) { elem->print(p); }, elems(), "[", "]");
    return p.stream();
}

std::ostream& PrimType::print(Printer& p) const { 
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return p.stream() << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}


/*
 * parameters
 */

std::ostream& TypeParam::print(Printer& p) const {
    p.stream() << symbol() << (bounds_.empty() ? "" : ": ");
    return p.dump_list([&] (const Type* type) { type->print(p); }, bounds(), "", "", " + ");
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
        type()->print(p);

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

    const FnType* ret = nullptr;
    if (!fn().params().empty() && fn().params().back()->symbol() == "return") {
        if (auto fntype = fn().params().back()->type()->isa<FnType>())
            ret = fntype;
    }

    p.stream() << '(';
    fn().print_params(p, ret != nullptr);
    p.stream() << ")";

    if (ret) {
        p.stream() << " -> ";
        if (ret->elems().size() == 1)
            ret->elem(0)->print(p);
        else
            p.dump_list([&] (const Type* type) { type->print(p); }, ret->elems(), "(", ")", ", ");
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
    return type()->print(p);
}

std::ostream& StructDecl::print(Printer& p) const {
    p.stream() << visibility().str() << "struct " << symbol();
    print_type_params(p) << " {";
    p.up();
    p.dump_list([&] (const FieldDecl* field) { field->print(p); }, fields(), "", "", ",", true);
    p.down() << "}";
    return p.stream();
}

std::ostream& Typedef::print(Printer& p) const {
    p.stream() << visibility().str() << "type " << symbol();
    print_type_params(p) << " = ";
    return type()->print(p) << ';';
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
    if (for_type()) {
        p.stream() << " for ";
        for_type()->print(p);
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
std::ostream& TupleExpr::print(Printer& p) const { return p.dump_list([&] (const Expr* expr) { expr->print(p); }, ops(), "(", ")"); }

std::ostream& DefiniteArrayExpr::print(Printer& p) const { 
    return p.dump_list([&] (const Expr* expr) { expr->print(p); }, ops(), "[", "]"); 
}

std::ostream& RepeatedDefiniteArrayExpr::print(Printer& p) const { 
    p.stream() << '[';
    value()->print(p) << ", .. ";
    return count()->print(p) << ']';
}

std::ostream& IndefiniteArrayExpr::print(Printer& p) const { 
    p.stream() << '[';
    size()->print(p) << ": ";
    return elem_type()->print(p) << ']';
}

std::ostream& PrefixExpr::print(Printer& p) const {
    Prec r = PrecTable::prefix_r[kind()];
    Prec old = p.prec;
    bool paren = !p.is_fancy();
    if (paren) p.stream() << "(";

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

    if (paren) p.stream() << ")";
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

std::ostream& CastExpr::print(Printer& p) const { 
    lhs()->print(p) << " as ";
    return as()->print(p);
}

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
        auto ret = fn().params().back()->as<FnType>();
        if (ret->elems().size() == 1)
            ret->elem(0)->print(p);
        else
            p.dump_list([&] (const Type* type) { type->print(p); }, ret->elems(), "(", ")", ", ");
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

//------------------------------------------------------------------------------

} // namespace impala
