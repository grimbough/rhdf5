

#include "HandleListcpp.h"
// extern "C" {
#include "HandleList.h"

void
    addHandle( hid_t id ) {
        addHandleCPP( id );
    }

SEXP _h5listIdentifier( ) {
    hsize_t n_max = idListSizeCPP();
    hid_t * validIDs = (hid_t *)R_alloc( n_max, sizeof(hid_t) );
    hsize_t n = validIdentifierCPP( validIDs, n_max );
    
    SEXP Rval = PROTECT(allocVector(VECSXP, 2));
    
    SEXP type = PROTECT(allocVector(INTSXP, n));
    SEXP name = PROTECT(allocVector(STRSXP, n));
    
    if (n > 0) {
        //    std::vector<hid_t>::iterator it;
        hsize_t i=0;
        H5I_type_t t;
        ssize_t st;
        for (i=0; i < n; i++) {
            hid_t id = validIDs[i];
            t = H5Iget_type(id);
            INTEGER(type)[i] = t;
            if ((t == H5I_FILE) || (t == H5I_GROUP) || (t == H5I_DATASET) || (t == H5I_ATTR)) {
                st = H5Iget_name( id, NULL, 0 );
                st = st+1;
                char n1[st];
                H5Iget_name( id, (char *)(&n1), st );
                SET_STRING_ELT(name, i, mkChar(n1));
            } else {
                SET_STRING_ELT(name, i, mkChar(""));
            }
        }
    }
    
    //free(validIDs);

    SET_VECTOR_ELT(Rval,0,type);
    SET_VECTOR_ELT(Rval,1,name);
    
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("type"));
    SET_STRING_ELT(names, 1, mkChar("name"));
    SET_NAMES(Rval, names);
    UNPROTECT(4);
    
    return(Rval);
}

SEXP _h5validObjects( ) {
    hsize_t n_max = idListSizeCPP();
    hid_t * validIDs = (hid_t *)R_alloc( n_max, sizeof(hid_t) );
    hsize_t n = validIdentifierCPP( validIDs, n_max );
    
    //SEXP Rval = PROTECT(allocVector(INTSXP, n));
    SEXP Rval = PROTECT(allocVector(STRSXP, n));
    if (n > 0) {
        hsize_t i;
        for (i=0; i < n; i++) {
            //INTEGER(Rval)[i] = validIDs[i];
            SET_STRING_ELT(Rval, i, HID_2_CHARSXP(validIDs[i]));
        }
    }
    UNPROTECT(1);
    
    return(Rval);
}


void
    removeHandle( hid_t id ) {
        removeHandleCPP( id );
    }

SEXP  handleInfoName( hid_t ID) {
    SEXP Rval = PROTECT(allocVector(VECSXP, 2));
    ssize_t st = H5Iget_name( ID, NULL, 0 );
    char n1[st+1];
    H5Iget_name( ID, (char *)(&n1), st+1 );
    SET_VECTOR_ELT(Rval, 0, mkString(n1));
    SET_VECTOR_ELT(Rval, 1, mkString(""));
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("name"));
    SET_STRING_ELT(names, 1, mkChar("filename"));
    SET_NAMES(Rval, names);
    UNPROTECT(2);
    return(Rval);
}

SEXP _handleInfo ( SEXP _ID ) {

    hid_t ID = STRSXP_2_HID(_ID);
    int isvalid = H5Iis_valid(ID);
    
    SEXP Rval = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(Rval,0,ScalarLogical(isvalid));
    H5I_type_t type = H5Iget_type(ID);
    SET_VECTOR_ELT(Rval,1,ScalarInteger(type));
    if (isvalid) {
        switch(type) {
        case H5I_FILE: case H5I_GROUP: case H5I_DATASET: case H5I_ATTR: {
            SET_VECTOR_ELT(Rval, 2, handleInfoName(ID));
        } break;
        default: {
            SET_VECTOR_ELT(Rval, 2, R_NilValue);
        }
        }
    } else {
        SET_VECTOR_ELT(Rval, 2, mkString(""));
    }
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("isvalid"));
    SET_STRING_ELT(names, 1, mkChar("type"));
    SET_STRING_ELT(names, 2, mkChar("info"));
    SET_NAMES(Rval, names);
    UNPROTECT(2);
    return Rval;
}

// }

