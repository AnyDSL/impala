#ifndef IMPALA_LOC_H
#define IMPALA_LOC_H

#include "thorin/util/stream.h"

namespace impala {

using thorin::Stream;

class Loc {
public:
    Loc() = default;

    Loc(const char* filename, uint32_t front_line, uint32_t front_col, uint32_t back_line, uint32_t back_col)
        : filename_(filename)
        , front_line_(front_line)
        , front_col_(front_col)
        , back_line_(back_line)
        , back_col_(back_col)
    {}
    Loc(const char* filename, uint32_t line, uint32_t col)
        : Loc(filename, line, col, line, col)
    {}
    Loc(Loc front, Loc back)
        : Loc(front.filename(), front.front_line(), front.front_col(), back.back_line(), back.back_col())
    {}

    const char* filename() const { return filename_; }
    uint32_t front_line() const { return front_line_; }
    uint32_t front_col() const { return front_col_; }
    uint32_t back_line() const { return back_line_; }
    uint32_t back_col() const { return back_col_; }

    Loc front() const { return {filename_, front_line(), front_col(), front_line(), front_col()}; }
    Loc back() const { return {filename_, back_line(), back_col(), back_line(), back_col()}; }
    bool is_set() const { return filename_ != nullptr; }

protected:
    const char* filename_ = nullptr; // TODO squeeze additional 4 * 4 bits out of the upper 16 bits
    uint16_t front_line_ = 1, front_col_ = 1, back_line_ = 1, back_col_ = 1;
};

Loc operator+(Loc l1, Loc l2);
Stream& operator<<(Stream&, Loc);

}

#endif
