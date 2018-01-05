### R code from vignette source '/Users/da42327_ca/devel/rhdf5/vignettes/rhdf5.Rnw'

###################################################
### code chunk number 1: style
###################################################
BiocStyle::latex()


###################################################
### code chunk number 2: installation (eval = FALSE)
###################################################
## source("http://bioconductor.org/biocLite.R")
## biocLite("rhdf5")

library(rhdf5)
h5file = H5Fcreate("newfile.h5")
h5file


###################################################
### code chunk number 25: createfile
###################################################
h5group1 <- H5Gcreate(h5file, "foo")
h5group2 <- H5Gcreate(h5file, "baa")
h5group3 <- H5Gcreate(h5group1, "foobaa")
h5group3


###################################################
### code chunk number 26: createdataspace
###################################################
d = c(5,7)
h5space1 = H5Screate_simple(d,d)
h5space2 = H5Screate_simple(d,NULL)
h5space3 = H5Scopy(h5space1)
h5space4 = H5Screate("H5S_SCALAR")
h5space1
H5Sis_simple(h5space1)


###################################################
### code chunk number 27: create dataset
###################################################
h5dataset1 = H5Dcreate( h5file, "dataset1", "H5T_IEEE_F32LE", h5space1 )
h5dataset2 = H5Dcreate( h5group2, "dataset2", "H5T_STD_I32LE", h5space1 )
h5dataset1


###################################################
### code chunk number 28: writedata
###################################################
A = seq(0.1,3.5,length.out=5*7)
H5Dwrite(h5dataset1, A)
B = 1:35
H5Dwrite(h5dataset2, B)


###################################################
### code chunk number 29: closefile
###################################################
H5Dclose(h5dataset1)
H5Dclose(h5dataset2)

H5Sclose(h5space1)
H5Sclose(h5space2)
H5Sclose(h5space3)
H5Sclose(h5space4)

H5Gclose(h5group1)
H5Gclose(h5group2)
H5Gclose(h5group3)

H5Fclose(h5file)


###################################################
### code chunk number 30: sessioninfo
###################################################
toLatex(sessionInfo())


