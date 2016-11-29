#include <fstream>
#include <vector>
#include <cctype>
#include <stdexcept>

#include "thorin/be/llvm/llvm.h"
#include "thorin/be/ycomp.h"
#include "thorin/util/args.h"
#include "thorin/util/log.h"
#include "thorin/util/location.h"
#include "thorin/util/ycomp.h"

#include "impala/ast.h"
#include "impala/cgen.h"
#include "impala/impala.h"

//------------------------------------------------------------------------------

using namespace thorin;
using namespace std;

typedef vector<string> Names;

//------------------------------------------------------------------------------

ostream* open(ofstream& stream, const string& name) {
    if (name == "-")
        return &cout;

    stream.open(name);
    return &stream;
}

int main(int argc, char** argv) {
    try {
        if (argc < 1)
            throw logic_error("bad number of arguments");

        string prgname = argv[0];
        Names infiles;
#ifndef NDEBUG
        Names breakpoints;
#endif
        string out_name, log_name, log_level;
        bool help,
             emit_cint, emit_thorin, emit_ast, emit_annotated, emit_llvm, emit_ycomp, emit_ycomp_cfg,
             opt_thorin, opt_s, opt_0, opt_1, opt_2, opt_3, debug,
             nocleanup, nossa, fancy;
        YCompCommandLine yComp;

        auto cmd_parser = ArgParser()
            .implicit_option             (                      "<infiles>",                      "input files", infiles)
            .add_option<bool>            ("help",               "",                               "produce this help message", help, false)
#ifndef NDEBUG
            .add_option<vector<string>>  ("break",              "<arg>",                          "breakpoint at definition generation of with global id <arg>; may be used multiple times", breakpoints)
            .add_option<string>          ("log-level",          "{none|error|warn|info|debug}",   "set log level", log_level, "warn")
            .add_option<string>          ("log",                "<arg>",                          "specifies log file; use '-' for stdout (default)", log_name, "-")
#endif
            .add_option<string>          ("o",                  "",                               "specifies the output module name", out_name, "")
            .add_option<bool>            ("O0",                 "",                               "reduce compilation time and make debugging produce the expected results (default)", opt_0, false)
            .add_option<bool>            ("O1",                 "",                               "optimize", opt_1, false)
            .add_option<bool>            ("O2",                 "",                               "optimize even more", opt_2, false)
            .add_option<bool>            ("O3",                 "",                               "optimize yet more", opt_3, false)
            .add_option<bool>            ("Os",                 "",                               "optimize for size", opt_s, false)
            .add_option<bool>            ("Othorin",            "",                               "optimize at Thorin level", opt_thorin, false)
            .add_option<bool>            ("emit-annotated",     "",                               "emit AST of Impala program after semantic analysis", emit_annotated, false)
            .add_option<bool>            ("emit-ast",           "",                               "emit AST of Impala program", emit_ast, false)
            .add_option<bool>            ("emit-c-interface",   "",                               "emit C interface from Impala code (experimental)", emit_cint, false)
            .add_option<bool>            ("emit-llvm",          "",                               "emit llvm from Thorin representation (implies -Othorin)", emit_llvm, false)
            .add_option<bool>            ("emit-thorin",        "",                               "emit textual Thorin representation of Impala program", emit_thorin, false)
            .add_option<bool>            ("emit-ycomp",         "",                               "emit ycomp-compatible graph representation of Impala program", emit_ycomp, false)
            .add_option<bool>            ("emit-ycomp-cfg",     "",                               "emit ycomp-compatible control-flow graph representation of Impala program", emit_ycomp_cfg, false)
            .add_option<bool>            ("f",                  "",                               "use fancy output: Impala's AST dump uses only parentheses where necessary", fancy, false)
            .add_option<bool>            ("g",                  "",                               "emit debug information", debug, false)
            .add_option<bool>            ("nocleanup",          "",                               "no clean-up phase", nocleanup, false)
            .add_option<bool>            ("nossa",              "",                               "use slots + load/store instead of SSA construction", nossa, false)
            .add_option<YCompCommandLine>("ycomp",              "{cfg|domtree|domfrontiers|looptree} {true|false} <arg>    ",
                "print ycomp graph to <arg>; the flag indicates whether the graph is based upon a forward (true) or backwards (false) CFG; the option can be specified multiple times",
                yComp, YCompCommandLine());

        // do cmdline parsing
        cmd_parser.parse(argc, argv);
        opt_thorin |= emit_llvm;

        impala::fancy() = fancy;

#ifndef NDEBUG
        ofstream log_stream;
        if (log_level == "none") {
            // do nothing
        } else if (log_level == "error") {
            Log::set(Log::Error, open(log_stream, log_name));
        } else if (log_level == "warn") {
            Log::set(Log::Warn, open(log_stream, log_name));
        } else if (log_level == "info") {
            Log::set(Log::Info, open(log_stream, log_name));
        } else if (log_level == "debug") {
            Log::set(Log::Debug, open(log_stream, log_name));
        } else
            throw invalid_argument("log level must be one of {none|error|warn|info|debug}");
#else
        Log::set(Log::Error, &std::cout);
#endif

        // check optimization levels
        if (opt_s + opt_0 + opt_1 + opt_2 + opt_3 > 1)
            throw invalid_argument("multiple optimization levels specified");

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
            for (const auto& infile : infiles) {
                auto i = infile.find_last_of('.');
                if (infile.substr(i + 1) != "impala")
                    throw invalid_argument("input file '" + infile + "' does not have '.impala' extension");
                auto rest = infile.substr(0, i);
                auto f = rest.find_last_of('/');
                if (f != string::npos) {
                    rest = rest.substr(f+1);
                }
                if (rest.empty())
                    throw invalid_argument("input file '" + infile + "' has empty module name");
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

        impala::Items items;
        for (const auto& infile : infiles) {
            auto filename = infile.c_str();
            ifstream file(filename);
            impala::parse(items, file, filename);
        }

        auto module = std::make_unique<const impala::Module>(infiles.front().c_str(), std::move(items));

        if (emit_ast)
            module->stream(std::cout);

        check(init, module.get(), nossa);
        bool result = impala::num_errors() == 0;

        if (result && emit_annotated)
            module->stream(std::cout);

        if (result && emit_cint) {
            impala::CGenOptions opts;

            size_t pos = module_name.find_last_of("\\/");
            pos = (pos == std::string::npos) ? 0 : pos + 1;
            opts.file_name = module_name.substr(pos) + ".h";

            // Generate a valid include guard macro name
            opts.guard = opts.file_name;
            if (!std::isalpha(opts.guard[0]) && opts.guard[0] != '_') opts.guard.insert(opts.guard.begin(), '_');
            transform(opts.guard.begin(), opts.guard.end(), opts.guard.begin(), [] (char c) -> char {
                if (!std::isalnum(c)) return '_';
                return ::toupper(c);
            });
            opts.guard[opts.guard.length() - 2] = '_';

            ofstream out_file(module_name + ".h");
            if (!out_file) {
                std::cerr << "cannot open file " << opts.file_name << "for writing" << std::endl;
                return EXIT_FAILURE;
            }
            impala::generate_c_interface(module.get(), opts, out_file);
        }

        if (result && (emit_llvm || emit_thorin || emit_ycomp || emit_ycomp_cfg))
            emit(init.world, module.get());

        if (result) {
            if (!nocleanup)
                init.world.cleanup();
            if (opt_thorin)
                init.world.opt();
            if (emit_thorin)      init.world.dump();
            if (emit_llvm)        thorin::emit_llvm(init.world, opt, debug);
            if (emit_ycomp)       thorin::emit_ycomp(init.world, true);
            if (emit_ycomp_cfg)   thorin::emit_ycomp_cfg(init.world);
            yComp.print(init.world);
        } else
            return EXIT_FAILURE;

        return EXIT_SUCCESS;
    } catch (exception const& e) {
        cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (...) {
        cerr << "unknown exception" << std::endl;
        return EXIT_FAILURE;
    }
}
