#include <iostream>

#include "impala/ast.h"

#include "impala/sema/trait.h"

namespace impala {

void TypeNode::dump() const { std::cout << to_string() << std::endl; }

std::string Trait::to_string() const { return is_error_trait() ? "<error trait>" : trait_decl()->symbol().str(); }

std::string TraitInstanceNode::to_string() const {
    std::string result = trait_->to_string();

    if (var_inst_size() == 0)
        return result;

    const char* separator = "[";
    for (auto v : var_instances_) {
        result += separator + v->to_string();
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
