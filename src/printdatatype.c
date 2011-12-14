#include "printdatatype.h"

char*
getDatatypeName(hid_t type) {
  char  *name=NULL;  /* byte order string */
    /* char       *mname; */
/*     hid_t       mtype, str_type; */
/*     unsigned    nmembers; */
/*     unsigned    ndims; */
/*     unsigned    i; */
/*     size_t      size=0; */
/*     hsize_t     dims[H5DUMP_MAX_RANK]; */
/*     H5T_str_t   str_pad; */
/*     H5T_cset_t  cset; */
  H5T_order_t order;
/*     hid_t       super; */
/*     hid_t       tmp_type; */
/*     htri_t      is_vlstr=FALSE; */
  const char  *order_s=NULL;  /* byte order string */
  H5T_sign_t  sign;           /* sign scheme value */
  const char  *sign_s=NULL;   /* sign scheme string */

  /* htri_t TRUE = H5Tequal( H5T_STD_I8LE, H5T_STD_I8LE ); */

  if (H5Tcommitted(type) > 0) {
/*         H5O_info_t  oinfo; */
/*         obj_t  *obj;    /\* Found object *\/ */

/*         H5Oget_info(type, &oinfo); */
/*         obj = search_obj(type_table, oinfo.addr); */

/*         if(obj) { */
/*             if(!obj->recorded) */
/*                 printf("\"/#%a\"", obj->objno); */
/*             else */
/*                 printf("\"%s\"", obj->objname); */
/*         } else { */
/*             printf("unknown committed type.\n"); */
/*             /\* h5tools_setstatus(EXIT_FAILURE); *\/ */
/*         } */
  } else {
    switch(H5Tget_class(type)) {
    case H5T_INTEGER:
      if(H5Tequal(type, H5T_STD_I8BE) == TRUE) {
	name = "H5T_STD_I8BE";
      } else if(H5Tequal(type, H5T_STD_I8LE) == TRUE) {
	name = "H5T_STD_I8LE";
      } else if(H5Tequal(type, H5T_STD_I16BE) == TRUE) {
	name = "H5T_STD_I16BE";
      } else if(H5Tequal(type, H5T_STD_I16LE) == TRUE) {
	name = "H5T_STD_I16LE";
      } else if(H5Tequal(type, H5T_STD_I32BE) == TRUE) {
	name = "H5T_STD_I32BE";
      } else if(H5Tequal(type, H5T_STD_I32LE) == TRUE) {
	name = "H5T_STD_I32LE";
      } else if(H5Tequal(type, H5T_STD_I64BE) == TRUE) {
	name = "H5T_STD_I64BE";
      } else if(H5Tequal(type, H5T_STD_I64LE) == TRUE) {
	name = "H5T_STD_I64LE";
      } else if(H5Tequal(type, H5T_STD_U8BE) == TRUE) {
	name = "H5T_STD_U8BE";
      } else if(H5Tequal(type, H5T_STD_U8LE) == TRUE) {
	name = "H5T_STD_U8LE";
      } else if(H5Tequal(type, H5T_STD_U16BE) == TRUE) {
	name = "H5T_STD_U16BE";
      } else if(H5Tequal(type, H5T_STD_U16LE) == TRUE) {
	name = "H5T_STD_U16LE";
      } else if(H5Tequal(type, H5T_STD_U32BE) == TRUE) {
	name = "H5T_STD_U32BE";
      } else if(H5Tequal(type, H5T_STD_U32LE) == TRUE) {
	name = "H5T_STD_U32LE";
      } else if(H5Tequal(type, H5T_STD_U64BE) == TRUE) {
	name = "H5T_STD_U64BE";
      } else if(H5Tequal(type, H5T_STD_U64LE) == TRUE) {
	name = "H5T_STD_U64LE";
      } else if(H5Tequal(type, H5T_NATIVE_SCHAR) == TRUE) {
	name = "H5T_NATIVE_SCHAR";
      } else if(H5Tequal(type, H5T_NATIVE_UCHAR) == TRUE) {
	name = "H5T_NATIVE_UCHAR";
      } else if(H5Tequal(type, H5T_NATIVE_SHORT) == TRUE) {
	name = "H5T_NATIVE_SHORT";
      } else if(H5Tequal(type, H5T_NATIVE_USHORT) == TRUE) {
	name = "H5T_NATIVE_USHORT";
      } else if(H5Tequal(type, H5T_NATIVE_INT) == TRUE) {
	name = "H5T_NATIVE_INT";
      } else if(H5Tequal(type, H5T_NATIVE_UINT) == TRUE) {
	name = "H5T_NATIVE_UINT";
      } else if(H5Tequal(type, H5T_NATIVE_LONG) == TRUE) {
	name = "H5T_NATIVE_LONG";
      } else if(H5Tequal(type, H5T_NATIVE_ULONG) == TRUE) {
	name = "H5T_NATIVE_ULONG";
      } else if(H5Tequal(type, H5T_NATIVE_LLONG) == TRUE) {
	name = "H5T_NATIVE_LLONG";
      } else if(H5Tequal(type, H5T_NATIVE_ULLONG) == TRUE) {
	name = "H5T_NATIVE_ULLONG";
      } else {
	
	/* byte order */
	if(H5Tget_size(type)>1) {
	  order = H5Tget_order(type);
	  if (H5T_ORDER_LE == order) {
	    order_s = " little-endian";
	  } else if (H5T_ORDER_BE == order) {
	    order_s = " big-endian";
	  } else if (H5T_ORDER_VAX == order) {
	    order_s = " mixed-endian";
	  } else {
	    order_s = " unknown-byte-order";
	  }
	} else {
	  order_s = "";
	}
	
	/* sign */
	if ((sign=H5Tget_sign(type))>=0) {
	  if (H5T_SGN_NONE == sign) {
	    sign_s = " unsigned";
	  } else if (H5T_SGN_2 == sign) {
	    sign_s = "";
	  } else {
	    sign_s = " unknown-sign";
	  }
	} else {
	  sign_s = " unknown-sign";
	}

	name = "unknown integer";
	/* TODO print integer format correctly to name*/
	/* print size, order, and sign */
	/* name = malloc((100+strlen(order_s)+strlen(sign_s))*sizeof(char)); */
	/* sprintf(name, "%lu-bit%s%s integer", */
	/* 	(unsigned long)(8*H5Tget_size(type)), order_s, sign_s); */
      }
      break;

    case H5T_FLOAT:
      if(H5Tequal(type, H5T_IEEE_F32BE) == TRUE) {
	name = "H5T_IEEE_F32BE";
      } else if(H5Tequal(type, H5T_IEEE_F32LE) == TRUE) {
	name = "H5T_IEEE_F32LE";
      } else if(H5Tequal(type, H5T_IEEE_F64BE) == TRUE) {
	name = "H5T_IEEE_F64BE";
      } else if(H5Tequal(type, H5T_IEEE_F64LE) == TRUE) {
	name = "H5T_IEEE_F64LE";
      } else if(H5Tequal(type, H5T_VAX_F32) == TRUE) {
	name = "H5T_VAX_F32";
      } else if(H5Tequal(type, H5T_VAX_F64) == TRUE) {
	name = "H5T_VAX_F64";
      } else if(H5Tequal(type, H5T_NATIVE_FLOAT) == TRUE) {
	name = "H5T_NATIVE_FLOAT";
      } else if(H5Tequal(type, H5T_NATIVE_DOUBLE) == TRUE) {
	name = "H5T_NATIVE_DOUBLE";
#if H5_SIZEOF_LONG_DOUBLE !=0
      } else if(H5Tequal(type, H5T_NATIVE_LDOUBLE) == TRUE) {
	name = "H5T_NATIVE_LDOUBLE";
#endif
      } else {
	
	/* byte order */
	if(H5Tget_size(type)>1) {
	  order = H5Tget_order(type);
	  if (H5T_ORDER_LE == order) {
	    order_s = " little-endian";
	  } else if (H5T_ORDER_BE == order) {
	    order_s = " big-endian";
	  } else if (H5T_ORDER_VAX == order) {
	    order_s = " mixed-endian";
	  } else {
	    order_s = " unknown-byte-order";
	  }
	} else {
	  order_s = "";
	}
	
	name = "unknown floating-point";
	/* TODO: print floating-point format correctly to name */
	/* print size and byte order */
	/* printf("%lu-bit%s floating-point", */
	/*        (unsigned long)(8*H5Tget_size(type)), order_s); */
	
      }
      break;

    case H5T_TIME:
      name = "H5T_TIME: not yet implemented";
      break;

    case H5T_STRING:
      name = "HST_STRING";
      /* TODO: print string format correctly to name */
/*                 /\* Make a copy of type in memory in case when TYPE is on disk, the size */
/*                  * will be bigger than in memory.  This makes it easier to compare */
/*                  * types in memory. *\/ */
/*                 tmp_type = H5Tcopy(type); */
/*                 size = H5Tget_size(tmp_type); */
/*                 str_pad = H5Tget_strpad(tmp_type); */
/*                 cset = H5Tget_cset(tmp_type); */
/*                 is_vlstr = H5Tis_variable_str(tmp_type); */

/*                 printf("H5T_STRING %s\n", dump_header_format->strblockbegin); */
/*                 indent += COL; */

/*                 indentation(indent + COL); */
/*                 if(is_vlstr) */
/*                     printf("%s H5T_VARIABLE;\n", STRSIZE); */
/*                 else */
/*                     printf("%s %d;\n", STRSIZE, (int) size); */

/*                 indentation(indent + COL); */
/*                 printf("%s ", STRPAD); */
/*                 if (str_pad == H5T_STR_NULLTERM) */
/*                     printf("H5T_STR_NULLTERM;\n"); */
/*                 else if (str_pad == H5T_STR_NULLPAD) */
/*                     printf("H5T_STR_NULLPAD;\n"); */
/*                 else if (str_pad == H5T_STR_SPACEPAD) */
/*                     printf("H5T_STR_SPACEPAD;\n"); */
/*                 else */
/*                     printf("H5T_STR_ERROR;\n"); */

/*                 indentation(indent + COL); */
/*                 printf("%s ", CSET); */

/*                 if (cset == H5T_CSET_ASCII) */
/*                     printf("H5T_CSET_ASCII;\n"); */
/*                 else */
/*                     printf("unknown_cset;\n"); */

/*                 str_type = H5Tcopy(H5T_C_S1); */
/*                 if(is_vlstr) */
/*                     H5Tset_size(str_type, H5T_VARIABLE); */
/*                 else */
/*                     H5Tset_size(str_type, size); */
/*                 H5Tset_cset(str_type, cset); */
/*                 H5Tset_strpad(str_type, str_pad); */

/*                 indentation(indent + COL); */
/*                 printf("%s ", CTYPE); */

/*                 /\* Check C variable-length string first. Are the two types equal? *\/ */
/*                 if (H5Tequal(tmp_type, str_type)) { */
/*                     printf("H5T_C_S1;\n"); */
/*                     goto done; */
/*                 } */

/*                 /\* Change the endianness and see if they're equal. *\/ */
/*                 order = H5Tget_order(tmp_type); */
/*                 if(order==H5T_ORDER_LE) */
/*                     H5Tset_order(str_type, H5T_ORDER_LE); */
/*                 else if(order==H5T_ORDER_BE) */
/*                     H5Tset_order(str_type, H5T_ORDER_BE); */

/*                 if (H5Tequal(tmp_type, str_type)) { */
/*                     printf("H5T_C_S1;\n"); */
/*                     goto done; */
/*                 } */

/*                 /\* If not equal to C variable-length string, check Fortran type. *\/ */
/*                 H5Tclose(str_type); */
/*                 str_type = H5Tcopy(H5T_FORTRAN_S1); */
/*                 H5Tset_cset(str_type, cset); */
/*                 H5Tset_size(str_type, size); */
/*                 H5Tset_strpad(str_type, str_pad); */

/*                 /\* Are the two types equal? *\/ */
/*                 if (H5Tequal(tmp_type, str_type)) { */
/*                     printf("H5T_FORTRAN_S1;\n"); */
/*                     goto done; */
/*                 } */

/*                 /\* Change the endianness and see if they're equal. *\/ */
/*                 order = H5Tget_order(tmp_type); */
/*                 if(order==H5T_ORDER_LE) */
/*                     H5Tset_order(str_type, H5T_ORDER_LE); */
/*                 else if(order==H5T_ORDER_BE) */
/*                     H5Tset_order(str_type, H5T_ORDER_BE); */

/*                 if (H5Tequal(tmp_type, str_type)) { */
/*                     printf("H5T_FORTRAN_S1;\n"); */
/*                     goto done; */
/*                 } */

/*                 /\* Type doesn't match any of above. *\/ */
/*                 printf("unknown_one_character_type;\n "); */
/*                 h5tools_setstatus(EXIT_FAILURE); */

/*     done: */
/*                 H5Tclose(str_type); */
/*                 H5Tclose(tmp_type); */

/*                 indent -= COL; */
/*                 indentation(indent + COL); */
/*                 printf("%s", dump_header_format->strblockend); */
      break;

    case H5T_BITFIELD:
      name = "HST_BITFIELD";
      /* TODO: print BITFIELD format correctly to name */

/*                 if (H5Tequal(type, H5T_STD_B8BE)==TRUE) { */
/*                     printf("H5T_STD_B8BE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B8LE)==TRUE) { */
/*                     printf("H5T_STD_B8LE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B16BE)==TRUE) { */
/*                     printf("H5T_STD_B16BE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B16LE)==TRUE) { */
/*                     printf("H5T_STD_B16LE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B32BE)==TRUE) { */
/*                     printf("H5T_STD_B32BE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B32LE)==TRUE) { */
/*                     printf("H5T_STD_B32LE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B64BE)==TRUE) { */
/*                     printf("H5T_STD_B64BE"); */
/*                 } else if (H5Tequal(type, H5T_STD_B64LE)==TRUE) { */
/*                     printf("H5T_STD_B64LE"); */
/*                 } else { */
/*                     printf("undefined bitfield"); */
/*                     h5tools_setstatus(EXIT_FAILURE); */
/*                 } */
      break;

    case H5T_OPAQUE:
      name = "HST_OPAQUE";
      /* TODO: print OPAQUE format correctly to name */
      
/*                 printf("\n"); */
/*                 indentation(indent + COL); */
/*                 printf("H5T_OPAQUE;\n"); */
/*                 indentation(indent + COL); */
/*                 printf("OPAQUE_TAG \"%s\";\n", H5Tget_tag(type)); */
/*                 indentation(indent); */
      break;

    case H5T_COMPOUND:
      name = "HST_COMPOUND";
      /* TODO: print COMPOUND format correctly to name */
/*                 nmembers = H5Tget_nmembers(type); */
/*                 printf("H5T_COMPOUND %s\n", dump_header_format->structblockbegin); */

/*                 for (i = 0; i < nmembers; i++) { */
/*                     mname = H5Tget_member_name(type, i); */
/*                     mtype = H5Tget_member_type(type, i); */
/*                     indentation(indent + COL); */

/*                     if (H5Tget_class(mtype) == H5T_COMPOUND) */
/*                         indent += COL; */

/*                     print_datatype(mtype,0); */

/*                     if (H5Tget_class(mtype) == H5T_COMPOUND) */
/*                         indent -= COL; */

/*                     printf(" \"%s\";\n", mname); */
/*                     free(mname); */
/*                 } */

/*                 indentation(indent); */
/*                 printf("%s", dump_header_format->structblockend); */
                break;

    case H5T_REFERENCE:
      name = "HST_REFERENCE";
      /* TODO: print REFERENCE format correctly to name */
/*                 printf("H5T_REFERENCE"); */
/*                 /\* The BNF document states that the type of reference should be */
/*                  * displayed after "H5T_REFERENCE". Therefore add the missing */
/*                  * reference type if the region command line option is used. This */
/*                  * reference type will not be displayed if the region option is not used. *\/ */
/*                 if(display_region) { */
/*                     if (H5Tequal(type, H5T_STD_REF_DSETREG)==TRUE) { */
/*                         printf(" { H5T_STD_REF_DSETREG }"); */
/*                     } */
/*                     else { */
/*                         printf(" { H5T_STD_REF_OBJECT }"); */
/*                     } */
/*                 } */
      break;

    case H5T_ENUM:
      name = "HST_ENUM";
      /* TODO: print ENUM format correctly to name */
/*                 printf("H5T_ENUM %s\n", dump_header_format->enumblockbegin); */
/*                 indent += COL; */
/*                 indentation(indent + COL); */
/*                 super = H5Tget_super(type); */
/*                 print_datatype(super,0); */
/*                 printf(";\n"); */
/*                 print_enum(type); */
/*                 indent -= COL; */
/*                 indentation(indent + COL); */
/*                 printf("%s", dump_header_format->enumblockend); */
      break;

    case H5T_VLEN:
      name = "HST_VLEN";
      /* TODO: print format correctly to name */
/*                 printf("H5T_VLEN %s ", dump_header_format->vlenblockbegin); */
/*                 super = H5Tget_super(type); */
/*                 print_datatype(super,0); */
/*                 H5Tclose(super); */

/*                 /\* Print closing *\/ */
/*                 printf("%s", dump_header_format->vlenblockend); */
      break;

    case H5T_ARRAY:
      name = "HST_ARRAY";
      /* TODO: print format correctly to name */
/*                 /\* Get array base type *\/ */
/*                 super = H5Tget_super(type); */

/*                 /\* Print lead-in *\/ */
/*                 printf("H5T_ARRAY { "); */

/*                 /\* Get array information *\/ */
/*                 ndims = H5Tget_array_ndims(type); */
/*                 H5Tget_array_dims2(type, dims); */

/*                 /\* Print array dimensions *\/ */
/*                 for (i = 0; i < ndims; i++) */
/*                     printf("[%d]", (int) dims[i]); */

/*                 printf(" "); */

/*                 /\* Print base type *\/ */
/*                 print_datatype(super,0); */

/*                 /\* Close array base type *\/ */
/*                 H5Tclose(super); */

/*                 /\* Print closing *\/ */
/*                 printf(" }"); */

      break;

    default:
      name = "unknown datatype";
/*                 h5tools_setstatus(EXIT_FAILURE); */
      break;
    }
  } /* end else */
  return(name);
}

char*
getDatatypeClass(hid_t type) {
  char  *name=NULL;  /* byte order string */
  switch(H5Tget_class(type)) {
  case H5T_INTEGER:   name = "INTEGER"; break;
  case H5T_FLOAT:     name = "FLOAT"; break;
  case H5T_TIME:      name = "TIME"; break;
  case H5T_STRING:    name = "STRING"; break;
  case H5T_BITFIELD:  name = "BITFIELD"; break;
  case H5T_OPAQUE:    name = "OPAQUE"; break;
  case H5T_COMPOUND:  name = "COMPOUND"; break;
  case H5T_REFERENCE: name = "REFERENCE"; break;
  case H5T_ENUM:      name = "ENUM"; break;
  case H5T_VLEN:      name = "VLEN"; break;
  case H5T_ARRAY:     name = "ARRAY"; break;
  default:            name = "unknown"; break;
  }
  return(name);
}

SEXP _getDatatypeName(SEXP _type) {
  hid_t type = INTEGER(_type)[0];
  SEXP Rval = mkString(getDatatypeName(type));
  return(Rval);
}

SEXP _getDatatypeClass(SEXP _type) {
  hid_t type = INTEGER(_type)[0];
  SEXP Rval = mkString(getDatatypeClass(type));
  return(Rval);
}

