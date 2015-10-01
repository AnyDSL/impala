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

class ModContents;

void init();
void destroy();

struct Init {
    Init(std::string module_name)
        : world(module_name)
    {
        init();
    }
    ~Init() { destroy(); }

    thorin::World world;
    thorin::AutoPtr<TypeTable> typetable;
};

bool parse(ModContents*, std::istream&, const std::string&);
bool name_analysis(const ModContents* mod);
bool type_analysis(Init&, const ModContents*, bool nossa);
bool check(Init&, const ModContents* mod, bool nossa);
void emit(thorin::World&, const ModContents*);

}

#endif
