#include <fstream>
#include <vector>
#include <cctype>
#include <stdexcept>

#include "thorin/be/codegen.h"
#include "thorin/be/c/c.h"
#ifdef LLVM_SUPPORT
#include "thorin/be/llvm/cpu.h"
#endif

#include "impala/ast.h"
#include "impala/args.h"

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
        Names use_breakpoints;
        bool track_history;
#endif
        std::string out_name, log_name, log_level, host_triple, host_cpu, host_attr, hls_flags;
        bool help,
             emit_c, emit_cint, emit_thorin, emit_ast, emit_annotated, emit_llvm,
             opt_thorin, opt_s, opt_0, opt_1, opt_2, opt_3, debug,
             nocleanup, fancy;

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
            .add_option<Names>           ("use-break",          "<args>", "breakpoint when using definition with global id <arg>; may be used multiple times separated by space or '_'", use_breakpoints)
            .add_option<bool>            ("track-history",      "", "track hisotry of names - useful for debugging", track_history, false)
#endif
            .add_option<std::string>     ("o",                  "", "specifies the output module name", out_name, "")
            .add_option<bool>            ("O0",                 "", "reduce compilation time and make debugging produce the expected results (default)", opt_0, false)
            .add_option<bool>            ("O1",                 "", "optimize", opt_1, false)
            .add_option<bool>            ("O2",                 "", "optimize even more", opt_2, false)
            .add_option<bool>            ("O3",                 "", "optimize yet more", opt_3, false)
            .add_option<bool>            ("Os",                 "", "optimize for size", opt_s, false)
            .add_option<bool>            ("Othorin",            "", "optimize at Thorin level", opt_thorin, false)
            .add_option<bool>            ("emit-annotated",     "", "emit AST of Impala program after semantic analysis", emit_annotated, false)
            .add_option<bool>            ("emit-ast",           "", "emit AST of Impala program", emit_ast, false)
            .add_option<bool>            ("emit-c",             "", "emit C from Thorin representation (implies -Othorin)", emit_c, false)
            .add_option<bool>            ("emit-c-interface",   "", "emit C interface from Impala code (experimental)", emit_cint, false)
            .add_option<bool>            ("emit-llvm",          "", "emit llvm from Thorin representation (implies -Othorin)", emit_llvm, false)
            .add_option<bool>            ("emit-thorin",        "", "emit textual Thorin representation of Impala program", emit_thorin, false)
            .add_option<std::string>     ("host-triple",        "", "emit llvm target code for the specified target triple", host_triple, "")
            .add_option<std::string>     ("host-cpu",           "", "emit llvm target code for the specified cpu type", host_cpu, "")
            .add_option<std::string>     ("host-attr",          "", "emit llvm target code with the specified attributes", host_attr, "")
            .add_option<std::string>     ("hls-flags",          "", "emit HLS code for the specified flags", hls_flags, "")
            .add_option<bool>            ("f",                  "", "use fancy output: Impala's AST dump uses only parentheses where necessary", fancy, false)
            .add_option<bool>            ("g",                  "", "emit debug information", debug, false)
            .add_option<bool>            ("nocleanup",          "", "no clean-up phase", nocleanup, false);

        // do cmdline parsing
        cmd_parser.parse(argc, argv);
        opt_thorin |= emit_llvm | emit_c;

        impala::fancy() = fancy;

        // check optimization levels
        if (opt_s + opt_0 + opt_1 + opt_2 + opt_3 > 1)
            throw std::invalid_argument("multiple optimization levels specified");

        int opt = 0;
        if (opt_s) opt = -1;
        else if (opt_1) opt = 1;
        else if (opt_2) opt = 2;
        else if (opt_3) opt = 3;

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

        thorin::World world(module_name);
        impala::init();

        std::ofstream log_stream;
        world.set(std::make_shared<thorin::Stream>(*open(log_stream, log_name)));

        if (false) {}
        else if (log_level == "error")   world.set(thorin::LogLevel::Error);
        else if (log_level == "warn")    world.set(thorin::LogLevel::Warn);
        else if (log_level == "info")    world.set(thorin::LogLevel::Info);
        else if (log_level == "verbose") world.set(thorin::LogLevel::Verbose);
        else if (log_level == "debug")   world.set(thorin::LogLevel::Debug);
        else throw std::invalid_argument("log level must be one of " LOG_LEVELS);

#if THORIN_ENABLE_CHECKS && !defined(NDEBUG)
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

        if (!set_breakpoints(    breakpoints, &thorin::World::    breakpoint)) return EXIT_FAILURE;
        if (!set_breakpoints(use_breakpoints, &thorin::World::use_breakpoint)) return EXIT_FAILURE;

        world.enable_history(track_history);
#endif

        impala::Items items;
        for (const auto& infile : infiles) {
            auto filename = infile.c_str();
            std::ifstream file(filename);
            impala::parse(items, file, filename);
        }

        auto module = std::make_unique<const impala::Module>(infiles.front().c_str(), std::move(items));

        if (emit_ast)
            module->dump();

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

        if (result && (emit_c || emit_llvm || emit_thorin))
            impala::emit(world, module.get());

        if (result) {
            //thorin::verify_mem(world);
            if (!nocleanup)
                world.cleanup();
            if (opt_thorin)
                world.opt();
            if (emit_thorin)
                world.dump();
            if (emit_c || emit_llvm) {
                thorin::DeviceBackends backends(world, opt, debug, hls_flags);
                auto emit_to_file = [&] (thorin::CodeGen& cg) {
                    auto name = module_name + cg.file_ext();
                    std::ofstream file(name);
                    if (!file)
                        throw std::runtime_error("cannot write '" + name + "': " + strerror(errno));
                    else
                        cg.emit_stream(file);
                };
                if (emit_c) {
                    thorin::Cont2Config kernel_configs;
                    thorin::c::CodeGen cg(world, kernel_configs, thorin::c::Lang::C99, debug, hls_flags);
                    emit_to_file(cg);
                }
#ifdef LLVM_SUPPORT
                if (emit_llvm) {
                    thorin::llvm::CPUCodeGen cg(world, opt, debug, host_triple, host_cpu, host_attr);
                    emit_to_file(cg);
                }
#endif
                for (auto& cg : backends.cgs) {
                    if (cg) emit_to_file(*cg);
                }
            }
        } else {
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    } catch (std::exception const& e) {
        thorin::errf("{}", e.what());
        return EXIT_FAILURE;
    } catch (...) {
        thorin::errf("unknown exception");
        return EXIT_FAILURE;
    }
}
