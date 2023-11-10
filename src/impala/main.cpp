#include <fstream>
#include <vector>
#include <cctype>
#include <stdexcept>
#include "thorin/fe/parser.h"
#include "thorin/util/sys.h"

#ifdef LLVM_SUPPORT
#include "thorin/be/llvm/llvm.h"
#endif
#include "thorin/driver.h"
#include "thorin/analyses/schedule.h"
#include "thorin/pass/optimize.h"

#include "impala/args.h"
#include "impala/ast.h"
#include "impala/cgen.h"
#include "impala/impala.h"

//------------------------------------------------------------------------------

typedef std::vector<std::string> Names;

//------------------------------------------------------------------------------

std::ostream* open(std::ofstream& stream, const std::string& name) {
    if (name == "-")
        return &std::cout;

    stream.open(name);
    return &stream;
}

int main(int argc, char** argv) {
    try {
        if (argc < 1)
            throw std::logic_error("bad number of arguments");

        std::string prgname = argv[0];
        Names infiles;
#ifndef NDEBUG
        Names breakpoints;
#endif
        std::string out_name, log_name, log_level;
        bool help, clos,
             emit_cint, emit_thorin, emit_ast, emit_annotated,
             emit_llvm, opt_thorin, debug, fancy;

#ifndef NDEBUG
#define LOG_LEVELS "{error|warn|info|verbose|debug}"
#else
#define LOG_LEVELS "{error|warn|info}"
#endif

        auto cmd_parser = impala::ArgParser()
            .implicit_option             (                      "<infiles>", "input files", infiles)
            .add_option<bool>            ("help",               "",          "produce this help message", help, false)
            .add_option<std::string>     ("log-level",          LOG_LEVELS,  "set log level", log_level, "error")
            .add_option<std::string>     ("log",                "<arg>", "specifies log file; use '-' for stdout (default)", log_name, "-")
#ifndef NDEBUG
            .add_option<Names>           ("break",              "<args>", "breakpoint at definition generation with global id <arg>; may be used multiple times separated by space or '_'", breakpoints)
#endif
            .add_option<std::string>     ("o",                  "", "specifies the output module name", out_name, "")
            .add_option<bool>            ("Othorin",            "", "optimize at Thorin level", opt_thorin, false)
            .add_option<bool>            ("clos",               "", "loads experimental 'clos' thorin dialect plugin", clos, false)
            .add_option<bool>            ("emit-annotated",     "", "emit AST of Impala program after semantic analysis", emit_annotated, false)
            .add_option<bool>            ("emit-ast",           "", "emit AST of Impala program", emit_ast, false)
            .add_option<bool>            ("emit-c-interface",   "", "emit C interface from Impala code (experimental)", emit_cint, false)
            .add_option<bool>            ("emit-llvm",          "", "emit llvm from Thorin representation (implies -Othorin)", emit_llvm, false)
            .add_option<bool>            ("emit-thorin",        "", "emit textual Thorin representation of Impala program", emit_thorin, false)
            .add_option<bool>            ("f",                  "", "use fancy output: Impala's AST dump uses only parentheses where necessary", fancy, false)
            .add_option<bool>            ("g",                  "", "emit debug information", debug, false);

        // do cmdline parsing
        cmd_parser.parse(argc, argv);
        opt_thorin |= emit_llvm;

        impala::fancy() = fancy;

        if (infiles.empty() && !help) {
            thorin::errf("no input files");
            return EXIT_FAILURE;
        }

        if (help) {
            thorin::outf("Usage: {} [options] file...", prgname);
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
                    throw std::invalid_argument("input file '" + infile + "' does not have '.impala' extension");
                auto rest = infile.substr(0, i);
                auto f = rest.find_last_of('/');
                if (f != std::string::npos) {
                    rest = rest.substr(f+1);
                }
                if (rest.empty())
                    throw std::invalid_argument("input file '" + infile + "' has empty module name");
                module_name = rest;
            }
        }

        thorin::Driver driver;
        std::ofstream log_stream;

        thorin::Log::Level lvl;
        if (log_level == "error")   lvl = thorin::Log::Level::Error;
        if (log_level == "warn")    lvl = thorin::Log::Level::Warn;
        if (log_level == "info")    lvl = thorin::Log::Level::Info;
        if (log_level == "verbose") lvl = thorin::Log::Level::Verbose;
        if (log_level == "debug")   lvl = thorin::Log::Level::Debug;

        driver.log().set(open(log_stream, log_name)).set(lvl);
        driver.flags().aggressive_lam_spec = true;

        auto& world = driver.world();
        world.set(world.sym(module_name));
        auto parser = thorin::Parser(world);

        if (auto path = thorin::sys::path_to_curr_exe())
            driver.add_search_path(path->parent_path().parent_path() / "thorin2" / "lib" / "thorin");

        if (clos) driver.load("clos");
        for (auto plugin : {"core", "mem", "compile", "opt", "math", "affine"}) driver.load(plugin);

        if (clos) parser.plugin("clos");
        for (auto plugin : {"core", "mem", "compile", "opt", "math", "affine"}) parser.plugin(plugin);

        impala::init();

#if defined(THORIN_ENABLE_CHECKS) && !defined(NDEBUG)
        auto set_breakpoints = [&](auto breakpoints, auto setter) {
            for (auto b : breakpoints) {
                assert(b.size() > 0);
                size_t num = 0;
                for (size_t i = 0, e = b.size(); i != e; ++i) {
                    char c = b[i];
                    if (c == '_') {
                        if (num != 0) {
                            std::invoke(setter, world, num);
                            num = 0;
                        }
                    } else if (std::isdigit(c)) {
                        num = num*10 + c - '0';
                    } else {
                        std::cerr << "invalid breakpoint '" << b << "'" << std::endl;
                        return false;
                    }
                }

                if (num != 0) std::invoke(setter, world, num);
            }

            return true;
        };

        if (!set_breakpoints(breakpoints, &thorin::World::breakpoint)) return EXIT_FAILURE;
#endif

        impala::Items items;
        std::deque<std::filesystem::path> inpaths;
        for (const auto& infile : infiles) {
            inpaths.emplace_back(infile);
            std::ifstream file(inpaths.back());
            impala::parse(items, file, &inpaths.back());
        }

        auto module = std::make_unique<const impala::Module>(&inpaths.front(), std::move(items));
        if (emit_ast) module->dump();

        std::unique_ptr<impala::TypeTable> typetable;
        impala::check(typetable, module.get());
        bool result = impala::num_errors() == 0;

        if (emit_annotated)
            module->dump();

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

            std::ofstream out_file(module_name + ".h");
            if (!out_file) {
                thorin::errf("cannot open file '{}' for writing", opts.file_name);
                return EXIT_FAILURE;
            }
            impala::generate_c_interface(module.get(), opts, out_file);
        }

        if (result && (emit_llvm || emit_thorin))
            impala::emit(world, module.get());

        if (result) {
            if (opt_thorin) thorin::optimize(world);
            if (emit_thorin) world.dump();
            if (emit_llvm) {
                if (auto backend = driver.backend("ll")) {
                    std::ofstream ofs(module_name + ".ll");
                    backend(world, ofs);
                } else
                    throw std::runtime_error("error: 'll' emitter not loaded. thorin 'mem' dialect not found?");
            }
        } else
            return EXIT_FAILURE;

        return EXIT_SUCCESS;
    } catch (std::exception const& e) {
        thorin::errln("{}", e.what());
        return EXIT_FAILURE;
    } catch (...) {
        thorin::errln("unknown exception");
        return EXIT_FAILURE;
    }
}
