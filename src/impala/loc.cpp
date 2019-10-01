#include "impala/loc.h"

#include "thorin/util/stream.h"

namespace impala {

Loc operator+(Loc l1, Loc l2) {
    return {l1.filename(), l1.front_line(), l1.front_col(), l2.back_line(), l2.back_col()};
}

Stream& operator<<(Stream& os, Loc l) {
#ifdef _MSC_VER
    return os << l.filename() << "(" << l.front_line() << ")";
#else // _MSC_VER
    os << l.filename() << ':';

    if (l.front_col() == uint16_t(-1) || l.back_col() == uint16_t(-1)) {
        if (l.front_line() != l.back_line())
            return os.streamf("{} - {}", l.front_line(), l.back_line());
        else
            return os.streamf("{}", l.front_line());
    }

    if (l.front_line() != l.back_line())
        return os.streamf("{} col {} - {} col {}", l.front_line(), l.front_col(), l.back_line(), l.back_col());

    if (l.front_col() != l.back_col())
        return os.streamf("{} col {} - {}", l.front_line(), l.front_col(), l.back_col());

    return os.streamf("{} col {}", l.front_line(), l.front_col());
#endif // _MSC_VER
}

}
