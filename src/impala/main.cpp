#include <fstream>
#include <vector>
#include <cctype>
#include <stdexcept>

#include "thorin/analyses/cfg.h"
#include "thorin/analyses/domtree.h"
#include "thorin/analyses/looptree.h"
#include "thorin/analyses/scope.h"
#include "thorin/analyses/verify.h"
#include "thorin/transform/import.h"
#include "thorin/transform/vectorize.h"
#include "thorin/transform/partial_evaluation.h"
#include "thorin/be/thorin.h"
#include "thorin/be/llvm/llvm.h"
#include "thorin/be/ycomp.h"
#include "thorin/util/args.h"

#include "impala/ast.h"
#include "impala/cgen.h"
#include "impala/dump.h"
#include "impala/impala.h"
#include "impala/location.h"

//------------------------------------------------------------------------------

using namespace thorin;
using namespace std;

typedef vector<string> Names;

//------------------------------------------------------------------------------

int main(int argc, char** argv) {
    try {
        if (argc < 1)
            throw logic_error("bad number of arguments");

        string prgname = argv[0];
        Names infiles;
#ifndef NDEBUG
        Names breakpoints;
#endif
        string out_name;
        bool help,
             emit_cint, emit_thorin, emit_ast, emit_annotated, emit_llvm,
             emit_domtree, emit_postdomtree, emit_looptree, emit_ycomp,
             fancy, 
             opt_thorin, opt_s, opt_0, opt_1, opt_2, opt_3,
             nocleanup, nossa;

        int vectorlength = 0;
        auto cmd_parser = ArgParser()
            .implicit_option("infiles", "input files", infiles)
            // specify options
            .add_option<bool>("help", "produce this help message", help, false)
            .add_option<string>("o", "specifies the output module name", out_name, "")
#ifndef NDEBUG
            .add_option<vector<string>>("break", "breakpoint at definition generation of number arg", breakpoints)
#endif
            .add_option<bool>("O0",                 "reduce compilation time and make debugging produce the expected results (default)", opt_0, false)
            .add_option<bool>("O1",                 "optimize", opt_1, false)
            .add_option<bool>("O2",                 "optimize even more", opt_2, false)
            .add_option<bool>("O3",                 "optimize yet more", opt_3, false)
            .add_option<bool>("Os",                 "optimize for size", opt_s, false)
            .add_option<bool>("Othorin",            "optimize at THORIN level", opt_thorin, false)
            .add_option<bool>("emit-annotated",     "emit AST of impala program after semantical analysis", emit_annotated, false)
            .add_option<bool>("emit-ast",           "emit AST of impala program", emit_ast, false)
            .add_option<bool>("emit-domtree",       "emit dom tree", emit_domtree, false)
            .add_option<bool>("emit-c-interface",   "emit C interface from impala code (experimental)", emit_cint, false)
            .add_option<bool>("emit-llvm",          "emit llvm from THORIN representation (implies -Othorin)", emit_llvm, false)
            .add_option<bool>("emit-looptree",      "emit loop tree", emit_looptree, false)
            .add_option<bool>("emit-postdomtree",   "emit dom tree", emit_postdomtree, false)
            .add_option<bool>("emit-thorin",        "emit textual THORIN representation of impala program", emit_thorin, false)
            .add_option<bool>("emit-ycomp",         "emit ycomp-compatible graph representation of impala program", emit_ycomp, false)
            .add_option<bool>("f",                  "use fancy output: impala's AST dump uses only parentheses where necessary", fancy, false)
            .add_option<bool>("nocleanup",          "no clean-up phase", nocleanup, false)
            .add_option<bool>("nossa",              "use slots + load/store instead of SSA construction", nossa, false)
            .add_option< int>("vectorize",          "run vectorizer on main with given vector length (experimental), arg=<vector length>", vectorlength, false);

        // do cmdline parsing
        cmd_parser.parse(argc, argv);
        opt_thorin |= emit_llvm;

        // check optimization levels
        if (opt_s + opt_0 + opt_1 + opt_2 + opt_3 > 1)
            throw logic_error("multiple optimization levels specified");

        int opt = 0;
        if (opt_s) opt = -1;
        else if (opt_1) opt = 1;
        else if (opt_2) opt = 2;
        else if (opt_3) opt = 3;

        if (infiles.empty() && !help) {
            std::cerr << "no input files" << std::endl;
            return EXIT_FAILURE;
        }

        if (help) {
            std::cout << "Usage: " + prgname + " [options] file..." << std::endl;
            cmd_parser.print_help();
            return EXIT_SUCCESS;
        }

        std::string module_name;
        if (out_name.length()) {
            module_name = out_name;
        } else {
            for (auto infile : infiles) {
                auto i = infile.find_last_of('.');
                if (infile.substr(i + 1) != "impala")
                    throw logic_error("input file '" + infile + "' does not have '.impala' extension");
                auto rest = infile.substr(0, i);
                auto f = rest.find_last_of('/');
                if (f != string::npos) {
                    rest = rest.substr(f+1);
                }
                if (rest.empty())
                    throw logic_error("input file '" + infile + "' has empty module name");
                module_name = rest;
            }
        }

        impala::Init init(module_name);

#ifndef NDEBUG
        for (auto b : breakpoints) {
            assert(b.size() > 0);
            size_t num = 0;
            for (size_t i = 0, e = b.size(); i != e; ++i) {
                char c = b[i];
                if (!std::isdigit(c)) {
                    std::cerr << "invalid breakpoint '" << b << "'" << std::endl;
                    return EXIT_FAILURE;
                }
                num = num*10 + c - '0';
            }

            init.world.breakpoint(num);
        }
#endif

        bool result = true;
        thorin::AutoPtr<impala::ModContents> prg = new impala::ModContents();
        for (auto infile : infiles) {
            std::string filename = infile.c_str();
            ifstream file(filename);
            prg->set_loc(impala::Location(filename, 1, 1, 1, 1));
            result &= impala::parse(prg, file, filename);
        }

        if (!prg->items().empty())
            prg->set_loc(impala::Location(prg->items().front()->pos1(), prg->items().back()->pos2()));
        else
            prg->set_loc(impala::Location(infiles.front(), 1, 1, 1, 1));

        if (emit_ast)
            impala::dump(prg, fancy);

        result &= check(init, prg, nossa);

        if (result && emit_annotated)
            impala::dump(prg, fancy);

        if (result && emit_cint) {
            impala::CGenOptions opts;
            opts.file_name = module_name + ".h";
            opts.guard = opts.file_name;
            transform(opts.guard.begin(), opts.guard.end(), opts.guard.begin(), ::toupper);
            opts.guard[opts.guard.length() - 2] = '_';

            ofstream out_file(opts.file_name);
            if (!out_file) {
                std::cerr << "cannot open file " << opts.file_name << "for writing" << std::endl;
                return EXIT_FAILURE;
            }
            impala::generate_c_interface(prg, opts, out_file);
        }

        if (result && (emit_llvm || emit_thorin || emit_ycomp))
            emit(init.world, prg);

        if (result) {
            if (!nocleanup)
                init.world.cleanup();
            if (opt_thorin)
                init.world.opt();
            //if (vectorlength != 0) {
                //Lambda* impala_main = top_level_lambdas(init.world)[0];
                //Scope scope(impala_main);
                //thorin::vectorize(scope, vectorlength);
                //init.world.cleanup();
            //}
            if (emit_thorin)      thorin::emit_thorin(init.world);
            if (emit_domtree)     Scope::for_each(init.world, [] (const Scope& scope) { scope.f_cfg().domtree().dump(); });
            if (emit_postdomtree) Scope::for_each(init.world, [] (const Scope& scope) { scope.b_cfg().domtree().dump(); });
            if (emit_looptree)    Scope::for_each(init.world, [] (const Scope& scope) { scope.f_cfg().looptree().dump(); });
            if (emit_llvm)        thorin::emit_llvm(init.world, opt);
            if (emit_ycomp)       thorin::emit_ycomp(init.world, true);
        } else
            return EXIT_FAILURE;

        return EXIT_SUCCESS;
    } catch (exception const& e) {
        cerr << e.what() << endl;
        return EXIT_FAILURE;
    } catch (...) {
        cerr << "unknown exception" << endl;
        return EXIT_FAILURE;
    }
}
