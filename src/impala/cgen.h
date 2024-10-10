#ifndef IMPALA_CGEN_H
#define IMPALA_CGEN_H

#include <iostream>

namespace impala {

struct CGenOptions {
    CGenOptions()
        : structs_only(false)
        , fns_only(false)
        , file_name("interface.h")
        , guard("INTERFACE_H") {}

    bool structs_only : 1;
    bool fns_only     : 1;
    std::string file_name;
    std::string guard;
};

/**
 * Generates a C interface from the contents of an Impala module.
 * The typechecking pass has to be run before a call to this function.
 *
 * @param mod The module contents.
 * @param opts C interface generation options.
 * @param o The stream to use as output.
 * @return true on success, otherwise false
 */
bool generate_c_interface(const Module* mod, const CGenOptions& opts = CGenOptions(), std::ostream& o = std::cout);

} // namespace impala

#endif
