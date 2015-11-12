#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <istream>
#include <string>

#include "thorin/world.h"

#include "impala/sema/typetable.h"

namespace thorin {
    class World;
}

namespace impala {

class ASTNode;
class ModContents;

void init();
void destroy();

struct Init {
    Init(std::string module_name)
        : world(std::move(module_name))
    {
        init();
    }
    ~Init() { destroy(); }

    thorin::World world;
    thorin::AutoPtr<TypeTable> typetable;
};

void parse(ModContents*, std::istream&, const char*);
void name_analysis(const ModContents*);
void type_inference(const ModContents*);
void type_analysis(Init&, const ModContents*, bool nossa);
void borrow_check(const ModContents*);
void check(Init&, const ModContents*);
void emit(thorin::World&, const ModContents*);

std::ostream& warn(const ASTNode* n);            ///< Emit warning while using \p n as \p Location.
std::ostream& warn(const thorin::Location& loc); ///< Emit warning at \p Location \p loc.
std::ostream& error(const ASTNode* n);           ///< Emit error while using \p n as \p Location.
std::ostream& error(const thorin::Location& loc);///< Emit error at \p Location \p loc.

int num_warnings();
int num_errors();

}

#endif
