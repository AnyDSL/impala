# Impala
An imperative and functional programming language.

## Build Instructions

```sh
git clone --recurse-submodules git@github.com:AnyDSL/impala.git -b t2
cd impala
mkdir build
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build -j $(nproc)
```
For a `Release` build simply use `-DCMAKE_BUILD_TYPE=Release`.
