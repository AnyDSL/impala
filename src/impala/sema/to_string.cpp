#include <iostream>

#include "impala/ast.h"

#include "impala/sema/trait.h"

namespace impala {

void Generic::dump() const { std::cout << to_string() << std::endl; }

std::string TraitNode::to_string() const { return is_error_trait() ? "<trait error>" : trait_decl()->symbol().str(); }

std::string TraitInstanceNode::to_string() const {
    std::string result = trait()->to_string();

    assert(!var_instances_.empty());
    assert(var_instances_.size() == trait()->num_bound_vars());
    const char* separator = "[";
    for (TypeVar tv : trait()->bound_vars()) {
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
    if (id_ < 26) {
        return std::string(1, 'A' + id_);
    } else {
        return std::string("Z") + std::to_string(id_);
    }
}

}
