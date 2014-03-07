#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <istream>
#include <string>

#include "thorin/world.h"

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
};

const ModContents* parse(bool& result, std::istream&, const std::string&);
bool name_analysis(const ModContents* mod);
bool type_analysis(const ModContents* mod, bool nossa);
bool check(const ModContents* mod, bool nossa);
bool emit(thorin::World&, const ModContents*);

}

#endif
