#ifndef IMPALA_LOC_H
#define IMPALA_LOC_H

#include "thorin/util/stream.h"

namespace impala {

using thorin::Stream;

class Loc {
public:
    Loc() = default;

    Loc(const char* file, uint32_t begin_row, uint32_t begin_col, uint32_t finis_row, uint32_t finis_col)
        : file_(file)
        , begin_row_(begin_row)
        , begin_col_(begin_col)
        , finis_row_(finis_row)
        , finis_col_(finis_col)
    {}
    Loc(const char* file, uint32_t row, uint32_t col)
        : Loc(file, row, col, row, col)
    {}
    Loc(Loc begin, Loc finis)
        : Loc(begin.file(), begin.begin_row(), begin.begin_col(), finis.finis_row(), finis.finis_col())
    {}

    const char* file() const { return file_; }
    uint32_t begin_row() const { return begin_row_; }
    uint32_t begin_col() const { return begin_col_; }
    uint32_t finis_row() const { return finis_row_; }
    uint32_t finis_col() const { return finis_col_; }

    Loc begin() const { return {file_, begin_row(), begin_col(), begin_row(), begin_col()}; }
    Loc finis() const { return {file_, finis_row(), finis_col(), finis_row(), finis_col()}; }
    bool is_set() const { return file_ != nullptr; }

protected:
    const char* file_ = nullptr; // TODO squeeze additional 4 * 4 bits out of the upper 16 bits
    uint16_t begin_row_ = 1, begin_col_ = 1, finis_row_ = 1, finis_col_ = 1;
};

Loc operator+(Loc l1, Loc l2);
Stream& operator<<(Stream&, Loc);

}

#endif
