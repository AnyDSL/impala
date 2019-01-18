#ifndef IMPALA_MPIGEN_H
#define IMPALA_MPIGEN_H

#include <impala/sema/type.h>
#include <unordered_set>

namespace impala {
    class MpiStructs {
    public:
        static MpiStructs& instance() {
            static MpiStructs _inst;
            return _inst;
        }
        ~MpiStructs() {}
        void addStruct(const StructType*);
        void emitMpiCode();

    private:
        MpiStructs() {}
        MpiStructs(const MpiStructs&);
        MpiStructs &operator = (const MpiStructs&);
        std::unordered_set<const StructType*> mpistructs;
        void emitHeader(std::stringstream& mpi_helper);
        void emitFooter(std::stringstream& mpi_helper);
        void emitStructHelper(const StructType* structType, std::stringstream& mpi_helper);
        void emitBufferHelper(std::stringstream& mpi_helper);
        void mpi_datatype(PrimTypeTag ptt, std::stringstream& mpi_helper);
    };
}

#endif //IMPALA_MPIGEN_H
