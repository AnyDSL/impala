#include "impala/loc.h"

#include "thorin/util/stream.h"

namespace impala {

Loc operator+(Loc l1, Loc l2) {
    return {l1.file(), l1.begin_row(), l1.begin_col(), l2.finis_row(), l2.finis_col()};
}

Stream& operator<<(Stream& os, Loc l) {
#ifdef _MSC_VER
    return os << l.file() << "(" << l.begin_row() << ")";
#else // _MSC_VER
    os << l.file() << ':';

    if (l.begin_col() == uint16_t(-1) || l.finis_col() == uint16_t(-1)) {
        if (l.begin_row() != l.finis_row())
            return os.fmt("{} - {}", l.begin_row(), l.finis_row());
        else
            return os.fmt("{}", l.begin_row());
    }

    if (l.begin_row() != l.finis_row())
        return os.fmt("{} col {} - {} col {}", l.begin_row(), l.begin_col(), l.finis_row(), l.finis_col());

    if (l.begin_col() != l.finis_col())
        return os.fmt("{} col {} - {}", l.begin_row(), l.begin_col(), l.finis_col());

    return os.fmt("{} col {}", l.begin_row(), l.begin_col());
#endif // _MSC_VER
}

}
