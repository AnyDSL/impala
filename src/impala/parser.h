#ifndef IMPALA_PARSER_H
#define IMPALA_PARSER_H

#include <istream>
#include <string>

namespace impala {

const ModContents* parse(bool& result, std::istream&, const std::string&);

}

#endif
