#ifndef IMPALA_PARSER_H
#define IMPALA_PARSER_H

#include <istream>
#include <string>

namespace impala {

class TypeTable;

const ModContents* parse(bool& result, TypeTable&, std::istream&, const std::string&);

} // namespace impala

#endif
