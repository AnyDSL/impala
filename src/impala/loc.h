#ifndef IMPALA_LOC_H
#define IMPALA_LOC_H

#include <filesystem>

#include "impala/stream.h"

namespace impala {

class Loc {
public:
    Loc() = default;

    Loc(const std::filesystem::path* file, uint16_t begin_row, uint16_t begin_col, uint16_t finis_row, uint16_t finis_col)
        : file_(file)
        , begin_row_(begin_row)
        , begin_col_(begin_col)
        , finis_row_(finis_row)
        , finis_col_(finis_col)
    {}
    Loc(const std::filesystem::path* file, uint16_t row, uint16_t col)
        : Loc(file, row, col, row, col)
    {}
    Loc(Loc begin, Loc finis)
        : Loc(begin.file(), begin.begin_row(), begin.begin_col(), finis.finis_row(), finis.finis_col())
    {}

    const std::filesystem::path* file() const { return file_; }
    uint16_t begin_row() const { return begin_row_; }
    uint16_t begin_col() const { return begin_col_; }
    uint16_t finis_row() const { return finis_row_; }
    uint16_t finis_col() const { return finis_col_; }

    Loc begin() const { return {file_, begin_row(), begin_col(), begin_row(), begin_col()}; }
    Loc finis() const { return {file_, finis_row(), finis_col(), finis_row(), finis_col()}; }
    bool is_set() const { return file_ != nullptr; }

protected:
    const std::filesystem::path* file_ = nullptr;
    uint16_t begin_row_ = 1, begin_col_ = 1, finis_row_ = 1, finis_col_ = 1;
};

Loc operator+(Loc l1, Loc l2);
Stream& operator<<(Stream&, Loc);

}

#endif
