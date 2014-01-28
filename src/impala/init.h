#ifndef IMPALA_INIT_H
#define IMPALA_INIT_H

#include "thorin/world.h"

namespace thorin {
    class World;
}

namespace impala {

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

} // namespace impala

#endif
