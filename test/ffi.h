#include <utility>
#include <type_traits>
#include <functional>
#include <assert.h>

template<typename R, typename... Args>
struct closure_base {
public:
    using ret_t = R;
    using fnc_t = ret_t (Args...);

    ret_t operator() (Args... args) {
        if constexpr (std::is_void_v<ret_t>) {
            (*fnc_)(env_, args...);
        } else {
            return (*fnc_)(env_, args...);
        }
    }

    operator std::function<fnc_t>() {
        if constexpr (std::is_void_v<ret_t>) {
            return [this](auto... args) { this(args...); };
        } else {
            return [this](auto... args) { return this(args...); };
        }
    }

protected:
    using env_t = void*;
    using hidden_func_t = ret_t (env_t, Args...);

    template<class Env>
    closure_base(Env env, R (*fnc)(Env, Args...))
        : env_((env_t) env), fnc_((hidden_func_t*) fnc) {
        static_assert(sizeof(env) <= sizeof(env_t));
    };

private:
    env_t env_;
    hidden_func_t* fnc_;
}__attribute__((packed));

template<class F> requires std::is_function_v<F>
struct closure {};

template<typename R, typename... Args>
struct closure<R (Args...)> : public closure_base<R, Args...> {
public:

    using base_t = closure_base<R, Args...>;

    static_assert(std::is_standard_layout_v<base_t>);
    static_assert(sizeof(base_t) == sizeof(typename base_t::env_t) + sizeof(typename base_t::hidden_func_t*));

    template<class F> requires std::is_invocable_v<F, Args...>
    closure(F* f) 
        : base_t(f, &ccall<F>) {};

private:
    template<typename F> requires std::is_invocable_v<F, Args...>
    static typename base_t::ret_t ccall(F* f, Args... args) {
        if constexpr (std::is_void_v<typename base_t::ret_t>) {
            (*f)(args...);
        } else {
            return (*f)(args...);
        }
    }
}__attribute__((packed));

inline void static_test(closure<void (int)> void_c, closure<int (int, float, char*)> int_c, std::function< void (closure<void(int)>) > accept_closure) {
    static_assert(sizeof(int_c) == sizeof(closure_base<int, int, float, char*>));
    void_c(int_c(0, 0.0, nullptr));
    auto cpp_clos = [](int _) { return; };
    accept_closure(&cpp_clos);
}
