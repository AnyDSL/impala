#include "impala/dump.h"

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/prec.h"

#include "thorin/util/printer.h"

using thorin::ArrayRef;
using thorin::Type;

namespace impala {

class Printer : public thorin::Printer {
public:
    Printer(std::ostream& o, bool fancy)
        : thorin::Printer(o, fancy)
        , prec(BOTTOM)
    {}

    Prec prec;
};


//------------------------------------------------------------------------------

void ASTNode::dump() const { Printer p(std::cout, true); print(p) << std::endl; }

//------------------------------------------------------------------------------

/*
 * paths
 */

std::ostream& PathElem::print(Printer& p) const { 
    p.stream() << symbol();
    if (!args().empty())
        p.dump_list([&] (const ASTType* type) { type->print(p); }, args(), "[", "]");

    return p.stream();
}

std::ostream& Path::print(Printer& p) const {
    p.stream() << (is_global() ? "::" : "");
    return p.dump_list([&] (const PathElem* path_elem) { path_elem->print(p); }, path_elems(), "", "", "::");
}

/*
 * types
 */

std::ostream& ErrorASTType::print(Printer& p) const { return p.stream() << "<error>"; }

std::ostream& PtrASTType::print(Printer& p) const {
    p.stream() << kind();
    return referenced_type()->print(p);
}

std::ostream& DefiniteArrayASTType::print(Printer& p) const {
    p.stream() << '[';
    return elem_type()->print(p) << " * " << dim() << ']';
}

std::ostream& IndefiniteArrayASTType::print(Printer& p) const {
    p.stream() << '[';
    return elem_type()->print(p) << ']';
}

std::ostream& TupleASTType::print(Printer& p) const {
    return p.dump_list([&] (const ASTType* elem) { elem->print(p); }, elems(), "(", ")");
}

std::ostream& FnASTType::print(Printer& p) const {
    auto ret = ret_fn_type();
    p.stream() << "fn";
    print_type_params(p);
    p.dump_list([&] (const ASTType* elem) { elem->print(p); }, ret != nullptr ? elems().slice_num_from_end(1) : elems(), "(", ")");
    if (ret != nullptr) {
        p.stream() << " -> ";
        ret->print(p);
    }
    return p.stream();
}

std::ostream& ASTTypeApp::print(Printer& p) const {
    p.stream() << symbol();
    if (!elems().empty())
        p.dump_list([&] (const ASTType* elem) { elem->print(p); }, elems(), "[", "]");
    return p.stream();
}

std::ostream& PrimASTType::print(Printer& p) const {
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
    return p.dump_list([&] (const ASTType* type) { type->print(p); }, bounds(), "", "", " + ");
}

std::ostream& TypeParamList::print_type_params(Printer& p) const {
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
    if (!is_anonymous()) {
        p.stream() << symbol();
        if (!type().empty()) {
            p.stream() << ": ";
            p.stream() << type();
        } else if (ast_type()) {
            p.stream() << ": ";
            ast_type()->print(p);
        }
    }

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

    const FnASTType* ret = nullptr;
    if (!params().empty() && params().back()->symbol() == "return") {
        if (auto fntype = params().back()->ast_type()->isa<FnASTType>())
            ret = fntype;
    }

    p.stream() << '(';
    print_params(p, ret != nullptr);
    p.stream() << ")";

    if (ret) {
        p.stream() << " -> ";
        if (ret->elems().size() == 1)
            ret->elem(0)->print(p);
        else
            p.dump_list([&] (const ASTType* type) { type->print(p); }, ret->elems(), "(", ")", ", ");
    }

    if (body()) {
        p.stream() << ' ';
        body()->print(p);
    } else
        p.stream() << ';';

    return p.stream();
}

std::ostream& FieldDecl::print(Printer& p) const {
    p.stream() << (is_mut() ? "mut " : "" ) << visibility().str() << symbol() << ": ";
    return ast_type()->print(p);
}

std::ostream& StaticItem::print(Printer& p) const {
    p.stream() << "static " << (is_mut() ? "mut " : "") << symbol() << ": ";
    type()->print(p) << " = ";
    return init()->print(p) << ";";
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
        p.dump_list([&] (const ASTTypeApp* type_app) { type_app->print(p); }, super());
    }
        
    p.stream() << " {";
    p.up();
    p.dump_list([&] (const FnDecl* method) { method->print(p); }, methods(), "", "", "", true);
    return p.down() << "}";
}

std::ostream& Impl::print(Printer& p) const {
    p.stream() << "impl";
    print_type_params(p) << ' ';
    if (trait())
        trait()->print(p) << " for ";
    for_type()->print(p);
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

std::ostream& Expr::print(Printer& p) const {
    vprint(p);
    if (!inferred_args().empty())
        return p.dump_list([&] (Type t) { p.stream() << t; }, inferred_args(), "[", "]", ", ", false);
    return p.stream();
}

std::ostream& BlockExpr::vprint(Printer& p) const {
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

std::ostream& LiteralExpr::vprint(Printer& p) const {
    switch (kind()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return p.stream() << box().get_##atype();
#include "impala/tokenlist.h"
        case LIT_bool: return p.stream() << (box().get_bool() ? "true" : "false");
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& PathExpr ::vprint(Printer& p) const { return path()->print(p); }
std::ostream& EmptyExpr::vprint(Printer& p) const { return p.stream() << "/*empty*/"; }
std::ostream& TupleExpr::vprint(Printer& p) const { return p.dump_list([&] (const Expr* expr) { expr->print(p); }, elems(), "(", ")"); }

std::ostream& DefiniteArrayExpr::vprint(Printer& p) const {
    return p.dump_list([&] (const Expr* expr) { expr->print(p); }, elems(), "[", "]"); 
}

std::ostream& RepeatedDefiniteArrayExpr::vprint(Printer& p) const {
    p.stream() << '[';
    value()->print(p) << ", .. ";
    return count()->print(p) << ']';
}

std::ostream& IndefiniteArrayExpr::vprint(Printer& p) const {
    p.stream() << '[';
    size()->print(p) << ": ";
    return elem_type()->print(p) << ']';
}

std::ostream& PrefixExpr::vprint(Printer& p) const {
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

std::ostream& InfixExpr::vprint(Printer& p) const {
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

std::ostream& PostfixExpr::vprint(Printer& p) const {
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

std::ostream& FieldExpr::vprint(Printer& p) const {
    lhs()->print(p) << '.'; 
    return path_elem()->print(p); 
}

std::ostream& CastExpr::vprint(Printer& p) const {
    lhs()->print(p) << " as ";
    return ast_type()->print(p);
}

std::ostream& StructExpr::vprint(Printer& p) const {
    path()->print(p) << '{';
    p.dump_list([&] (const Elem& elem) { p.stream() << elem.symbol() << ": "; elem.expr()->print(p); }, elems());

    return p.stream() << '}';
}

std::ostream& MapExpr::vprint(Printer& p) const {
    lhs()->print(p);
    return p.dump_list([&](const Expr* expr) { expr->print(p); }, args(), "(", ")");
}

std::ostream& FnExpr::vprint(Printer& p) const {
    p.stream() << '|';
    print_params(p, has_return_type_);
    p.stream() << "| ";

    if (has_return_type_) {
        p.stream() << "-> ";
        auto ret = params().back();
        if (!ret->type().empty()) {
            auto rettype = ret->type().as<FnType>();
            if (rettype->elems().size() == 1)
                p.stream() << rettype->elem(0);
            else
                p.dump_list([&] (Type type) { p.stream() << type; }, rettype->elems(), "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->elems().size() == 1)
                rettype->elem(0)->print(p);
            else
                p.dump_list([&] (const ASTType* type) { type->print(p); }, rettype->elems(), "(", ")", ", ");
        }
        p.stream() << " ";
    }

    return body()->print(p);
}

std::ostream& IfExpr::vprint(Printer& p) const {
    p.stream() << "if ";
    cond()->print(p) << " ";
    then_expr()->print(p);
    if (has_else()) {
        p.stream() << " else ";
        else_expr()->print(p);
    }
    return p.stream();
}

std::ostream& ForExpr::vprint(Printer& p) const {
    p.stream() << "for ";
    p.dump_list([&](const Param* param) { param->print(p); }, params()) << " in ";
    expr()->print(p) << ' ';
    return body()->print(p);
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

}
