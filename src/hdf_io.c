#include "common.h"

/* utility functions to read data in to HDF5 format */

SEXP HDF_readCEL(SEXP group, SEXP name, SEXP create)
{
    FILE *f;
    char *filename, buf[600];
    int cols, rows, i, j, xp, yp, lines, ns;
    double mn, std, npix, *mnmat, *sdmat, *npmat;

    hid_t space, data, dtype;
    hsize_t	dims[2];

    if(!isGROUP(group) )
	error("first argument is not a group");

    if(!isString(name) )
	error("second argument is not a name");

    filename = CHAR(STRING_ELT(name,0));

    if( !(f = fopen(filename, "r")) )
	error("file %s could not be opened \n",filename);

    fgets(buf, 600, f);
    if( strncmp(buf,"[CEL]", 5) ) {
	fclose(f);
	error("file %s does not appear to be a CEL file", filename);
    }
    lines = 1;
    while( strncmp(buf, "Cols", 4) ) {
	fgets(buf, 600, f);
	lines++;
    }

    cols = atoi((char *) buf+5);
    if( cols < 100 || cols > 10000 ) {
	fclose(f);
	error("file %s does not seem right", filename);
    }

    while( strncmp(buf, "Rows", 4) ) {
	fgets(buf, 600, f);
	lines++;
    }
    
    rows = atoi((char *) buf+5);
    
    if( rows < 100 || rows > 10000 ) {
	fclose(f);
	error("file %s does not seem right", filename);
    }
    
    while( strncmp(buf, "CellHeader", 10) ) {
	fgets(buf, 600, f);
	lines++;
    }

    mnmat = (double *) R_alloc(rows*cols, sizeof(double));
    sdmat = (double *) R_alloc(rows*cols, sizeof(double));
    npmat = (double *) R_alloc(rows*cols, sizeof(double));

    for(i=0; i<rows; i++) {
	for(j=0; j<cols; j++) {
	    ns = fscanf(f, "%d %d %lf %lf %lf", &xp, &yp, &mn, &std, &npix);
	    lines++;
	    if( ns != 5 )
		error("failure on line %d of file %s", lines,
		      filename);
	    mnmat[xp+yp*(cols-1)] = mn;
	    sdmat[xp+yp*(cols-1)] = std;
	    npmat[xp+yp*(cols-1)] = npix;
	}
    }


    dtype = H5T_NATIVE_DOUBLE;
    dims[0] = (hsize_t) rows; dims[1] = (hsize_t) cols;

    space = H5Screate_simple(2,dims,NULL);
    if(space < 0 )
	error("unable to create simple dataspace");

    data = H5Dcreate(HID(group),"Mean", dtype, space, HID(create));
    if( data < 0 )
	error("unable to create Mean dataset");
    H5Dwrite(data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, mnmat);
    HDF_setRtype(data, REALSXP);
    H5Dclose(data);

    data = H5Dcreate(HID(group),"Stdev", dtype, space, HID(create));
    if( data < 0 )
	error("unable to create Stdev dataset");
    H5Dwrite(data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, sdmat);
    HDF_setRtype(data, REALSXP);
    H5Dclose(data);
    
    data = H5Dcreate(HID(group),"Npix", dtype, space, HID(create));
    if( data < 0 )
	error("unable to create Npix dataset");
    H5Dwrite(data, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, npmat);
    HDF_setRtype(data, REALSXP);
    H5Dclose(data);

    fclose(f);

    return R_NilValue;
}




