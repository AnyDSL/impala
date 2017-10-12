#include <fstream>
#include <vector>
#include <cctype>
#include <stdexcept>

#ifdef LLVM_SUPPORT
#include "thorin/be/llvm/llvm.h"
#endif
#include "thorin/util/args.h"
#include "thorin/util/log.h"
#include "thorin/util/location.h"

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
        bool track_history;
#endif
        string out_name, log_name, log_level;
        bool help,
             emit_cint, emit_thorin, emit_ast, emit_annotated,
             emit_llvm, opt_thorin, opt_s, opt_0, opt_1, opt_2, opt_3, debug,
             nocleanup, nossa, simple_pe, fancy;

#ifndef NDEBUG
#define LOG_LEVELS "{error|warn|info|verbose|debug}"
#else
#define LOG_LEVELS "{error|warn|info}"
#endif

        auto cmd_parser = ArgParser()
            .implicit_option             (                      "<infiles>", "input files", infiles)
            .add_option<bool>            ("help",               "",          "produce this help message", help, false)
            .add_option<string>          ("log-level",          LOG_LEVELS,  "set log level", log_level, "error")
            .add_option<string>          ("log",                "<arg>", "specifies log file; use '-' for stdout (default)", log_name, "-")
#ifndef NDEBUG
            .add_option<vector<string>>  ("break",              "<arg>", "breakpoint at definition generation of with global id <arg>; may be used multiple times", breakpoints)
            .add_option<bool>             ("track-history",     "", "track hisotry of names - useful for debugging", track_history, false)
#endif
            .add_option<string>          ("o",                  "", "specifies the output module name", out_name, "")
            .add_option<bool>            ("O0",                 "", "reduce compilation time and make debugging produce the expected results (default)", opt_0, false)
            .add_option<bool>            ("O1",                 "", "optimize", opt_1, false)
            .add_option<bool>            ("O2",                 "", "optimize even more", opt_2, false)
            .add_option<bool>            ("O3",                 "", "optimize yet more", opt_3, false)
            .add_option<bool>            ("Os",                 "", "optimize for size", opt_s, false)
            .add_option<bool>            ("Othorin",            "", "optimize at Thorin level", opt_thorin, false)
            .add_option<bool>            ("emit-annotated",     "", "emit AST of Impala program after semantic analysis", emit_annotated, false)
            .add_option<bool>            ("emit-ast",           "", "emit AST of Impala program", emit_ast, false)
            .add_option<bool>            ("emit-c-interface",   "", "emit C interface from Impala code (experimental)", emit_cint, false)
            .add_option<bool>            ("emit-llvm",          "", "emit llvm from Thorin representation (implies -Othorin)", emit_llvm, false)
            .add_option<bool>            ("emit-thorin",        "", "emit textual Thorin representation of Impala program", emit_thorin, false)
            .add_option<bool>            ("f",                  "", "use fancy output: Impala's AST dump uses only parentheses where necessary", fancy, false)
            .add_option<bool>            ("g",                  "", "emit debug information", debug, false)
            .add_option<bool>            ("nocleanup",          "", "no clean-up phase", nocleanup, false)
            .add_option<bool>            ("nossa",              "", "use slots + load/store instead of SSA construction", nossa, false)
            .add_option<bool>            ("simple-pe",          "", "use syntax instead of the CFG to determine when to stop PE", simple_pe, false);

        // do cmdline parsing
        cmd_parser.parse(argc, argv);
        opt_thorin |= emit_llvm;

        impala::fancy() = fancy;

        ofstream log_stream;
        if (log_level == "error") {
            Log::set(Log::Error, open(log_stream, log_name));
        } else if (log_level == "warn") {
            Log::set(Log::Warn, open(log_stream, log_name));
        } else if (log_level == "info") {
            Log::set(Log::Info, open(log_stream, log_name));
        } else if (log_level == "verbose") {
            Log::set(Log::Verbose, open(log_stream, log_name));
        } else if (log_level == "debug") {
            Log::set(Log::Debug, open(log_stream, log_name));
        } else
            throw invalid_argument("log level must be one of " LOG_LEVELS);

        // check optimization levels
        if (opt_s + opt_0 + opt_1 + opt_2 + opt_3 > 1)
            throw invalid_argument("multiple optimization levels specified");

        int opt = 0;
        if (opt_s) opt = -1;
        else if (opt_1) opt = 1;
        else if (opt_2) opt = 2;
        else if (opt_3) opt = 3;

        if (infiles.empty() && !help) {
            errf("no input files\n");
            return EXIT_FAILURE;
        }

        if (help) {
            outf("Usage: {} [options] file...\n", prgname);
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

        init.world.enable_history(track_history);
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

        if (emit_annotated)
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
                errf("cannot open file '{}' for writing\n", opts.file_name);
                return EXIT_FAILURE;
            }
            impala::generate_c_interface(module.get(), opts, out_file);
        }

        if (result && (emit_llvm || emit_thorin))
            emit(init.world, module.get());

        if (result) {
            if (!nocleanup)
                init.world.cleanup();
            if (opt_thorin)
                init.world.opt(simple_pe);
            if (emit_thorin)
                init.world.dump();
            if (emit_llvm) {
#ifdef LLVM_SUPPORT
                thorin::emit_llvm(init.world, opt, debug);
#else
                outf("warning: built without LLVM support - I don't emit an LLVM file\n");
#endif
            }
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
