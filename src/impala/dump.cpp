#include "impala/dump.h"

#include "impala/ast.h"
#include "impala/dump.h"

namespace impala {

//------------------------------------------------------------------------------

/*
 * types
 */

std::ostream& Unifiable::print_type_vars(Printer& p) const {
    if (is_polymorphic())
        return p.dump_list([&] (TypeVar type_var) {
                    type_var->print(p);
                    if (type_var->num_bounds() != 0) {
                        p.stream() << ": ";
                        p.dump_list([&] (TraitApp bound) { bound->print(p); }, type_var->bounds(), "", "", " + ");
                    }
                }, type_vars(), "[", "]");
    return p.stream();
}

std::ostream& UnknownTypeNode::print(Printer& p) const {
    assert(!is_unified());
    return p.stream() << '?' << id();
}

std::ostream& PrimTypeNode::print(Printer& p) const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return p.stream() << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& TypeErrorNode::print(Printer& p) const { return p.stream() << "<type error>"; }
std::ostream& NoRetTypeNode::print(Printer& p) const { return p.stream() << "<no-return>"; }

std::ostream& FnTypeNode::print(Printer& p) const {
    p.stream() << "fn";
    print_type_vars(p);
    Type ret_type = return_type();
    if (ret_type->is_noret())
        return p.dump_list([&] (Type type) { type->print(p); }, args(), "(", ")");

    p.dump_list([&] (Type type) { p.stream() << type; }, args().slice_num_from_end(1), "(", ")");
    p.stream() << " -> ";
    return ret_type->print(p);
}

std::ostream& TypeVarNode::print(Printer& p) const {
    if (!name_.empty())
        return p.stream() << name_.str();
    return p.stream() << '_' << id() << '_';
}

std::ostream& TraitAbsNode::print(Printer& p) const {
    return p.stream() << (is_error() ? "<trait error>" : trait_decl()->symbol().str());
}

std::ostream& TraitAppNode::print(Printer& p) const {
    if (is_error())
        return p.stream() << "<bound error>";

    p.stream() << trait();
    if (num_args() > 1)
        return p.dump_list([&] (Type type) { type->print(p); }, args().slice_from_begin(1), "[", "]");
    return p.stream();
}

std::ostream& OwnedPtrTypeNode::print(Printer& p)    const { p.stream() << '~'; return referenced_type()->print(p); }
std::ostream& BorrowedPtrTypeNode::print(Printer& p) const { p.stream() << '&'; return referenced_type()->print(p); }

std::ostream& DefiniteArrayTypeNode::print(Printer& p) const {
    p.stream() << '[';
    return elem_type()->print(p) << " * " << dim() << ']';
}

std::ostream& IndefiniteArrayTypeNode::print(Printer& p) const {
    p.stream() << '[';
    return elem_type()->print(p) << "]";
}

std::ostream& StructAbsTypeNode::print(Printer& p) const {
    return p.stream() << struct_decl_->symbol().str();
}

std::ostream& StructAppTypeNode::print(Printer& p) const {
    p.stream() << struct_abs_type()->struct_decl()->symbol().str();
    if (num_args() != 0)
        return p.dump_list([&] (Type type) { type->print(p); }, args(), "[", "]");
    return p.stream();
}

std::ostream& TypedefAbsNode::print(Printer& p) const {
    assert(num_type_vars() > 0); // otherwise no TypedefAbsNode should have been used in the first place
    p.stream() << "type";
    print_type_vars(p);
    p.stream() << " = ";
    return type()->print(p);
}

std::ostream& TupleTypeNode::print(Printer& p) const {
    print_type_vars(p);
    return p.dump_list([&] (Type type) { type->print(p); }, args(), "(", ")");
}

std::ostream& ImplNode::print(Printer& p) const {
    return p.stream() << "TODO";
}

//------------------------------------------------------------------------------

/*
 * AST types
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
    return p.dump_list([&] (const ASTType* elem) { elem->print(p); }, args(), "(", ")");
}

std::ostream& FnASTType::print(Printer& p) const {
    auto ret = ret_fn_type();
    p.stream() << "fn";
    print_type_params(p);
    p.dump_list([&] (const ASTType* arg) { arg->print(p); }, ret != nullptr ? args().slice_num_from_end(1) : args(), "(", ")");
    if (ret != nullptr) {
        p.stream() << " -> ";
        if (ret->num_args() == 1) {
            ret->args().front()->print(p);
        } else {
            p.dump_list([&] (const ASTType* arg) { arg->print(p); }, ret->args(), "(", ")");
        }
    }
    return p.stream();
}

std::ostream& ASTTypeApp::print(Printer& p) const {
    p.stream() << symbol();
    if (num_args() != 0)
        p.dump_list([&] (const ASTType* arg) { arg->print(p); }, args(), "[", "]");
    return p.stream();
}

std::ostream& PrimASTType::print(Printer& p) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return p.stream() << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& Typeof::print(Printer& p) const {
    p.stream() << "typeof(";
    return p.print(expr()) << ')';
}

/*
 * paths
 */

std::ostream& Identifier::print(Printer& p) const {
    return p.stream() << symbol();
}

std::ostream& PathElem::print(Printer& p) const {
    return p.stream() << symbol();
}

std::ostream& Path::print(Printer& p) const {
    p.stream() << (is_global() ? "::" : "");
    return p.dump_list([&] (const PathElem* path_elem) { path_elem->print(p); }, path_elems(), "", "", "::");
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
    return p.dump_list([&] (const Param* param) {
            if (!param->is_anonymous())
                p.stream() << (param->is_mut() ? "mut " : "") << param->symbol() << 
                    ((param->ast_type() || param->type()) ? ": " : "");
            if (auto type = param->type())
                p.stream() << type;
            else if (auto ast_type = param->ast_type())
                ast_type->print(p);
        },
        returning ? params().slice_num_from_end(1) : params());
}

std::ostream& ValueDecl::print(Printer& p) const {
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
 * items + item helpers
 */

std::ostream& ModContents::print(Printer& p) const {
    return p.dump_list([&] (const Item* item) { item->print(p); p.newline(); }, items(), "", "", "", true);
}

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

std::ostream& ExternBlock::print(Printer& p) const {
    p.stream() << "extern ";
    if (!abi_.empty())
        p.stream() << abi_.str() << ' ';
    p.stream() << '{';
    p.up();
    p.dump_list([&] (const FnDecl* fn) { fn->print(p); }, fns(), "", "", "", true);
    return p.down() << '}';
}

std::ostream& FnDecl::print(Printer& p) const {
    if (is_extern())
        p.stream() << "extern ";
    p.stream() << "fn ";
    if (export_name_)
        p.stream() << export_name_->symbol() << ' ';
    p.stream() << symbol();
    print_type_params(p);

    const FnASTType* ret = nullptr;
    if (!params().empty() && params().back()->symbol() == "return") {
        if (auto fn_type = params().back()->ast_type()->isa<FnASTType>())
            ret = fn_type;
    }

    p.stream() << '(';
    print_params(p, ret != nullptr);
    p.stream() << ")";

    if (ret) {
        p.stream() << " -> ";
        if (ret->num_args() == 1)
            ret->arg(0)->print(p);
        else
            p.dump_list([&] (const ASTType* type) { type->print(p); }, ret->args(), "(", ")", ", ");
    }

    if (body()) {
        p.stream() << ' ';
        p.print(body());
    } else
        p.stream() << ';';

    return p.stream();
}

std::ostream& FieldDecl::print(Printer& p) const {
    p.stream() << visibility().str() << symbol() << ": ";
    return ast_type()->print(p);
}

std::ostream& StaticItem::print(Printer& p) const {
    p.stream() << "static ";
    ValueDecl::print(p);
    if (init()) {
        p.stream() << " = ";
        p.print(init());
    }
    return p.stream() << ";";
}

std::ostream& StructDecl::print(Printer& p) const {
    p.stream() << visibility().str() << "struct " << symbol();
    print_type_params(p) << " {";
    p.up();
    p.dump_list([&] (const FieldDecl* field) { field->print(p); }, field_decls(), "", "", ",", true);
    p.down() << "}";
    return p.stream();
}

std::ostream& Typedef::print(Printer& p) const {
    p.stream() << visibility().str() << "type " << symbol();
    print_type_params(p) << " = ";
    return ast_type()->print(p) << ';';
}

std::ostream& TraitDecl::print(Printer& p) const {
    p.stream() << "trait " << symbol();
    print_type_params(p);

    if (!super_traits().empty()) {
        p.stream() << " : ";
        p.dump_list([&] (const ASTTypeApp* type_app) { type_app->print(p); }, super_traits());
    }

    p.stream() << " {";
    p.up();
    p.dump_list([&] (const FnDecl* method) { method->print(p); }, methods(), "", "", "", true);
    return p.down() << "}";
}

std::ostream& ImplItem::print(Printer& p) const {
    p.stream() << "impl";
    print_type_params(p) << ' ';
    if (trait())
        trait()->print(p) << " for ";
    ast_type()->print(p);
    p.stream() << " {";
    p.up();
    p.dump_list([&] (const FnDecl* method) { method->print(p); }, methods(), "", "", "", true);
    return p.down() << "}";
}

/*
 * expressions
 */

std::ostream& Printer::print(const Expr* expr) {
    return expr->print(*this);
}

std::ostream& SizeofExpr::print(Printer& p) const {
    p.stream() << "sizeof(";
    return ast_type()->print(p) << ')';
}

std::ostream& BlockExprBase::print(Printer& p) const {
    p.stream() << prefix();
    if (empty())
        return p.newline() << '}';
    p.up();
    p.dump_list([&] (const Stmt* stmt) { stmt->print(p); }, stmts(), "", "", "", true);
    if (!expr()->isa<EmptyExpr>()) {
        if (!stmts().empty())
            p.newline();
        p.print(expr());
    }

    return p.down() << "}";
}

std::ostream& LiteralExpr::print(Printer& p) const {
    switch (kind()) {
        case LIT_i8:  return p.stream() << box().get_s8()  << "i8";
        case LIT_i16: return p.stream() << box().get_s16() << "i16";
        case LIT_i32: return p.stream() << box().get_s32();
        case LIT_i64: return p.stream() << box().get_s64() << "i64";
        case LIT_u8:  return p.stream() << box().get_s8()  << "u8";
        case LIT_u16: return p.stream() << box().get_s16() << "u16";
        case LIT_u32: return p.stream() << box().get_s32() << "u";
        case LIT_u64: return p.stream() << box().get_s64() << "u64";
        case LIT_f32: return p.stream() << box().get_f32() << "f";
        case LIT_f64: return p.stream() << box().get_f64() << "f64";
        case LIT_bool: return p.stream() << (box().get_bool() ? "true" : "false");
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& CharExpr::print(Printer& p) const {
    return p.stream() << symbol();
}

std::ostream& StrExpr::print(Printer& p) const {
    if (symbols().size() == 1)
        return p.stream() << '\'' << symbols().front().remove_quotation() << '\'';
    p.up();
    p.dump_list([&] (Symbol symbol) { p.stream() << symbol; }, symbols() , "", "", "", true);
    return p.down();
}

std::ostream& PathExpr ::print(Printer& p) const { return path()->print(p); }
std::ostream& EmptyExpr::print(Printer& p) const { return p.stream() << "/*empty*/"; }
std::ostream& TupleExpr::print(Printer& p) const { return p.dump_list([&] (const Expr* expr) { p.print(expr); }, args(), "(", ")"); }

std::ostream& DefiniteArrayExpr::print(Printer& p) const {
    return p.dump_list([&] (const Expr* expr) { p.print(expr); }, args(), "[", "]");
}

std::ostream& RepeatedDefiniteArrayExpr::print(Printer& p) const {
    p.stream() << '[';
    return p.print(value()) << ", .. " << count() << ']';
}

std::ostream& IndefiniteArrayExpr::print(Printer& p) const {
    p.stream() << '[';
    p.print(dim()) << ": ";
    return elem_type()->print(p) << ']';
}

std::ostream& PrefixExpr::print(Printer& p) const {
    Prec r = PrecTable::prefix_r[kind()];
    Prec old = p.prec;
    bool paren = !p.is_fancy() || p.prec <= r;
    if (paren) p.stream() << "(";

    const char* op;
    switch (kind()) {
#define IMPALA_PREFIX(tok, str, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }

    p.stream() << op;
    p.prec = r;
    p.print(rhs());
    p.prec = old;

    if (paren) p.stream() << ")";
    return p.stream();
}

static std::pair<Prec, bool> open(Printer& p, int kind) {
    std::pair<Prec, bool> result;
    Prec l = PrecTable::postfix_l[kind];
    result.first = p.prec;
    result.second = !p.is_fancy() || p.prec > l;
    if (result.second)
        p.stream() << "(";
    p.prec = l;
    return result;
}

static std::ostream& close(Printer& p, std::pair<Prec, bool> pair) {
    p.prec = pair.first;
    if (pair.second)
        p.stream() << ")";
    return p.stream();
}

std::ostream& InfixExpr::print(Printer& p) const {
    auto open_state = open(p, kind());
    p.print(lhs());

    const char* op;
    switch (kind()) {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) case tok: op = str; break;
#define IMPALA_INFIX(     tok, str, lprec, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
    }

    p.stream() << " " << op << " ";

    p.prec = PrecTable::infix_r[kind()];
    p.print(rhs());
    return close(p, open_state);
}

std::ostream& PostfixExpr::print(Printer& p) const {
    auto open_state = open(p, kind());
    p.print(lhs());

    const char* op;
    switch (kind()) {
        case INC: op = "++"; break;
        case DEC: op = "--"; break;
        default: THORIN_UNREACHABLE;
    }

    p.stream() << op;
    return close(p, open_state);
}

std::ostream& FieldExpr::print(Printer& p) const {
    auto open_state = open(p, Token::DOT);
    p.print(lhs()) << '.' << symbol();
    return close(p, open_state);
}

std::ostream& CastExpr::print(Printer& p) const {
    auto open_state = open(p, Token::AS);
    p.print(lhs()) << " as ";
    ast_type()->print(p);
    return close(p, open_state);
}

std::ostream& TypeArgs::print_type_args(Printer& p) const {
    if (num_type_args() != 0)
        return p.dump_list([&](const ASTType* type) { type->print(p); }, type_args(), "[", "]");
    return p.stream();
}

std::ostream& StructExpr::print(Printer& p) const {
    path()->print(p);
    if (num_inferred_args() == 0)
        print_type_args(p);
    else
        p.dump_list([&] (Type t) { p.stream() << t; }, inferred_args(), "[", "]", ", ", false);
    return p.dump_list([&] (const Elem& elem) { p.stream() << elem.symbol() << ": "; p.print(elem.expr()); }, elems(), "{", "}");
}

std::ostream& MapExpr::print(Printer& p) const {
    Prec l = PrecTable::postfix_l[Token::L_PAREN];
    Prec old = p.prec;
    bool paren = !p.is_fancy() || p.prec > l;
    if (paren) p.stream() << "(";

    p.prec = l;
    p.print(lhs());
    if (num_inferred_args() == 0)
        print_type_args(p);
    else
        p.dump_list([&] (Type t) { p.stream() << t; }, inferred_args(), "[", "]", ", ", false);
    p.dump_list([&](const Expr* expr) { p.print(expr); }, args(), "(", ")");
    p.prec = old;
    if (paren) p.stream() << ")";
    return p.stream();
}

std::ostream& FnExpr::print(Printer& p) const {
    bool has_return_type = !params().empty() && params().back()->symbol() == "return";
    p.stream() << '|';
    print_params(p, has_return_type);
    p.stream() << "| ";

    if (has_return_type) {
        p.stream() << "-> ";
        auto ret = params().back();
        if (!ret->type().empty()) {
            auto rettype = ret->type().as<FnType>();
            if (rettype->num_args() == 1)
                p.stream() << rettype->arg(0);
            else
                p.dump_list([&] (Type type) { p.stream() << type; }, rettype->args(), "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->num_args() == 1)
                rettype->arg(0)->print(p);
            else
                p.dump_list([&] (const ASTType* type) { type->print(p); }, rettype->args(), "(", ")", ", ");
        }
        p.stream() << " ";
    }

    return p.print(body());
}

std::ostream& IfExpr::print(Printer& p) const {
    p.stream() << "if ";
    p.print(cond()) << ' ';
    p.print(then_expr());
    if (has_else()) {
        p.stream() << " else ";
        p.print(else_expr());
    }
    return p.stream();
}

std::ostream& WhileExpr::print(Printer& p) const {
    p.stream() << "while ";
    p.print(cond()) << ' ';
    return p.print(body());
}

std::ostream& ForExpr::print(Printer& p) const {
    p.stream() << "for ";
    p.dump_list([&](const Param* param) { param->print(p); }, fn_expr()->params().slice_num_from_end(1)) << " in ";
    p.print(expr()) << ' ';
    return p.print(fn_expr()->body());
}

/*
 * statements
 */

std::ostream& ItemStmt::print(Printer& p) const { return item()->print(p); }

std::ostream& LetStmt::print(Printer& p) const {
    p.stream() << "let ";
    local()->print(p);
    if (init()) {
        p.stream() << " = ";
        p.print(init());
    }
    return p.stream() << ';';
}

std::ostream& ExprStmt::print(Printer& p) const {
    bool no_semi = expr()->isa<IfExpr>() || expr()->isa<ForExpr>();
    p.print(expr());
    if (!no_semi)
        p.stream() << ';';
    return p.stream();
}

//------------------------------------------------------------------------------

void ASTNode::dump() const { Printer p(std::cout, true); print(p) << std::endl; }
void Unifiable::dump() const { Printer p(std::cout, true); print(p) << std::endl; }

void dump(const ASTNode* n, bool fancy, std::ostream& o) { Printer p(o, fancy); n->print(p); }
std::ostream& operator << (std::ostream& o, const ASTNode* n) { Printer p(o, true); return n->print(p); }
std::ostream& operator << (std::ostream& o, const Uni& uni) { Printer p(o, true); return uni->print(p); }

//------------------------------------------------------------------------------

}
