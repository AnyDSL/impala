#include <iostream>

#include "impala/ast.h"

#include "impala/sema/trait.h"

namespace impala {

void Generic::dump() const { std::cout << to_string() << std::endl; }

std::string TraitNode::to_string() const { return is_error_trait() ? "<trait error>" : trait_decl()->symbol().str(); }

std::string TraitInstanceNode::to_string() const {
    std::string result = trait()->to_string();

    assert(!var_instances_.empty());
    if (var_instances_.size() == 1)
        return result;

    assert(var_instances_.size() == trait()->num_bound_vars());
    const char* separator = "[";
    for (size_t i = 1; i < trait()->num_bound_vars(); ++i) {
        TypeVar tv = trait()->bound_var(i);
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

std::string CompoundType::elems_to_string() const {
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
        return std::string("_") + std::to_string(id_) + std::string("_");
    }
}

}
