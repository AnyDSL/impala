#include <iostream>

#include "impala/ast.h"

#include "impala/sema/trait.h"

namespace impala {

void Unifiable::dump() const { std::cout << to_string() << std::endl; }

std::string Unifiable::type_vars_to_string() const {
    std::string result;

    if (!is_generic())
        return result;

    const char* separator = "[";
    for (auto v : type_vars()) {
        result += separator + v->to_string();

        const UniSet<Trait> restr = v->bounds();

        if (!restr.empty()) {
            auto inner_sep = ":";
            for (auto t : restr) {
                result += inner_sep + t->to_string();
                inner_sep = "+";
            }
        }

        separator = ",";
    }
    return result + ']';
}

//std::string UnknownTypeNode::to_string() const { return is_instantiated() ? std::string("[") + std::to_string(id_) + instance()->to_string() + "]" : (std::string("?") + std::to_string(id_)); }
std::string UnknownTypeNode::to_string() const { return is_instantiated() ? instance()->to_string() : (std::string("?") + std::to_string(id())); }
std::string TraitNode::to_string() const { return is_error_trait() ? "<trait error>" : trait_decl()->symbol().str(); }

std::string TraitInstanceNode::to_string() const {
    std::string result = trait()->to_string();

    assert(!var_instances_.empty());
    if (var_instances_.size() == 1)
        return result;

    assert(var_instances_.size() == trait()->num_type_vars());
    const char* separator = "[";
    for (size_t i = 1; i < trait()->num_type_vars(); ++i) {
        TypeVar tv = trait()->type_var(i);
        // CHECK is node() correct here?
        assert(var_instances_.find(tv.node()) != var_instances_.end());
        result += separator + var_instances_.find(tv.node())->second->to_string();
        separator = ",";
    }

    return result + "]";
}

std::string PrimTypeNode::to_string() const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::string CompoundTypeNode::elems_to_string() const {
    std::string result;

    if (is_empty())
        return "()";

    const char* separator = "(";
    for (auto elem : elems_) {
        result += separator + elem->to_string();
        separator = ", ";
    }
    return result + ')';
}

std::string TypeVarNode::to_string() const {
    if (name_) {
        return name_.str();
    } else {
        return std::string("_") + std::to_string(id()) + std::string("_");
    }
}

}
