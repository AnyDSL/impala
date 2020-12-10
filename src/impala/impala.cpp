#include <fstream>

#include "impala/impala.h"

#include "thorin/util/log.h"
#include "thorin/util/symbol.h"

#include "impala/ast.h"
#include "impala/token.h"

namespace impala {

int global_num_warnings = 0;
int global_num_errors = 0;
bool fancy_output = false;

bool& fancy() { return fancy_output; }
int& num_warnings() { return global_num_warnings; }
int& num_errors() { return global_num_errors; }

void init() {
    PrecTable::init();
    Token::init();
}

void check(std::unique_ptr<TypeTable>& typetable, const Module* mod, bool nossa) {
    name_analysis(mod);
    type_inference(typetable, mod);
    type_analysis(mod, nossa);
    //borrow_check(mod);
}

Prec PrecTable::infix[Token::Num];

void PrecTable::init() {
#define IMPALA_INFIX(     tok, t_str, prec) PrecTable::infix[Token::tok] = Prec::prec;
#define IMPALA_INFIX_ASGN(tok, t_str)       PrecTable::infix[Token::tok] = Prec::Assign;
#include "impala/tokenlist.h"
}

}

/// Entry-point for the JIT in the runtime system.
bool compile(const std::vector<std::string>& file_names, const std::vector<std::string>& file_data, thorin::World& world, thorin::Log::Level log_level, std::ostream& error_stream) {
    static bool initalized = false;
    if (!initalized) {
        impala::init();
        initalized = true;
    }
    impala::num_warnings() = 0;
    impala::num_errors()   = 0;

    thorin::Log::set(log_level, &error_stream);

    impala::Items items;
    for (size_t n = file_names.size(), i = 0; i < n; ++i) {
        auto file_name = file_names[i];
        auto file_src  = file_data[i];
        std::istringstream program_is(file_src);
        impala::parse(items, program_is, file_name.c_str());
    }

    auto module = std::make_unique<const impala::Module>(file_names.back().c_str(), std::move(items));

    std::unique_ptr<impala::TypeTable> typetable;
    impala::check(typetable, module.get(), false);
    bool result = impala::num_errors() == 0;
    if (result)
        impala::emit(world, module.get());

    return result;
}
