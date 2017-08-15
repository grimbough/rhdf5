## Collection of tests that have been written with the expectation that they 
## work or produce useful error messages, but at the moment are not handled
## in a graceful fashion.  Updating the the package code to get these to pass
## is not a priority, but will improve the user experience IMHO

############################################################
context("h5write")
############################################################

D <- 1:20
h5createFile(h5File)
h5createDataset(file = h5File, dataset = "foo", dims = c(2,length(D) ))

expect_message( h5write(obj = D, file = h5File, name = "foo"),
                "dimensions of 'obj' and the 'foo' dataset are different" )