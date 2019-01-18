
#include "mpigen.h"
#include <sstream>

namespace impala {

    void impala::MpiStructs::addStruct(const StructType * st) {
        mpistructs.insert(st);
    }

    void MpiStructs::emitMpiCode() {
        std::stringstream mpi_helper;
        if(!mpi_helper) {
            std::cerr << "Could not open mpi_helper.cpp!" << std::endl;
        }
        emitHeader(mpi_helper);

        for (const auto& elem: mpistructs) {
            if(elem->num_ops() == 3 && elem->to_string() == "Buffer") {
                emitBufferHelper(mpi_helper);
            }
            else {
                emitStructHelper(elem, mpi_helper);
            }
        }
        emitFooter(mpi_helper);

        std::cerr << mpi_helper.str();
    }

    void MpiStructs::emitHeader(std::stringstream &mpi_helper) {
        mpi_helper << "#include <mpi.h>\n\n"
                      "#ifdef __cplusplus\n"
                      "extern \"C\" {\n"
                      "#endif\n\n";
    }

    void MpiStructs::emitFooter(std::stringstream &mpi_helper) {
        mpi_helper << "\n#ifdef __cplusplus\n"
                      "}\n"
                      "#endif\n";
    }

    void MpiStructs::emitStructHelper(const StructType* structType, std::stringstream &mpi_helper) {
        mpi_helper << "void mpi_type_" << structType->to_string() << "(int* impala_mpi_type";

        for(size_t i=0; i<structType->num_ops(); i++) {
            mpi_helper << ", void* arg" << i << "_ptr";
        }
        mpi_helper << ") {\n";
        //create new MPI_Datatype
        mpi_helper << "\tMPI_Datatype mpistructure;\n";
        mpi_helper << "\tint blocklengths[" << structType->num_ops() << "];\n";
        mpi_helper << "\tMPI_Datatype types[" << structType->num_ops() << "];\n";
        mpi_helper << "\tMPI_Aint displacements[" << structType->num_ops() << "];\n";
        mpi_helper << "\tint mpi_status = 0;\n";
        mpi_helper << "\tMPI_Initialized(&mpi_status);\n";
        mpi_helper << "\tif(mpi_status != 1) { MPI_Init(0,0); }\n";

        for(size_t i=0; i<structType->num_ops(); i++) {
            if(auto primType = structType->op(i)->isa<PrimType>()) {
                mpi_helper << "\tblocklengths[" << i << "] = 1;\n";
                mpi_helper << "\ttypes[" << i << "] = ";
                mpi_datatype(primType->primtype_tag(),mpi_helper);
            }
            else if(auto arrayType = structType->op(i)->isa<DefiniteArrayType>()) {
                //definite array
                mpi_helper << "\tblocklengths[" << i << "] = " << arrayType->dim() << ";\n";
                if(auto arrayPrimType = arrayType->elem_type()->isa<PrimType>()) {
                    mpi_helper << "\ttypes[" << i << "] = ";
                    mpi_datatype(arrayPrimType->primtype_tag(),mpi_helper);
                }
                else {
                    std::cerr << "Invalid type inside definite array!\n";
                }
            }
            else {
                std::cerr << "Invalid struct member in mpi_type call!\n";
                exit(1);
            }
            if(i == 0) {
                mpi_helper << "\tdisplacements[0] = 0;\n";
            }
            else {
                mpi_helper << "\tdisplacements[" << i << "] = (char*)arg" << i <<
                           "_ptr - (char*)arg0_ptr;\n";
            }
        }

        mpi_helper << "\tMPI_Type_create_struct(" <<structType->num_ops() <<
                   ",blocklengths, displacements, types, &mpistructure);\n";
        mpi_helper << "\tMPI_Type_commit(&mpistructure);\n";
        mpi_helper << "\t*impala_mpi_type = mpistructure;\n";
        mpi_helper << "}\n";
    }

    void MpiStructs::emitBufferHelper(std::stringstream& mpi_helper) {
        mpi_helper << "void mpi_type_Buffer(int* impala_mpi_type, void* data, int* size, int* device) {\n";
        mpi_helper << "\tMPI_Datatype array_type;\n";
        mpi_helper << "\tMPI_Type_contiguous(*size, MPI_BYTE, &array_type);\n";
        mpi_helper << "\tMPI_Type_commit(&array_type);\n";
        mpi_helper << "\t*impala_mpi_type = array_type;\n";
        mpi_helper << "}\n";
    }

    void MpiStructs::mpi_datatype(PrimTypeTag ptt, std::stringstream &mpi_helper) {
        switch(ptt) {
            case PrimType_i32: mpi_helper << "MPI_INT;\n"; break;
            case PrimType_f64: mpi_helper << "MPI_DOUBLE;\n"; break;
            case PrimType_i8: mpi_helper << "MPI_CHAR;\n"; break;
            default: {
                std::cerr << "Invalid primType struct member in mpi_type call!\n";
                exit(1);
            }
        }
    }

}

