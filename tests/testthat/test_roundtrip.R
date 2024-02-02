## These tests are intended to check if we write something to HDF5 and then 
## read back into R, we get the same thing. 
## Current caveat is that we typically return an array even when there is only
## one dimension.

h5file <- tempfile(pattern = "h5_roundtrip_", fileext = ".h5")

test_that("data.frame columns survive a round trip", {

    set.seed(1234)
        
    test_frame <- data.frame(
        integer = as.integer(1:10),
        ## random words of length 5-20
        words = vapply(1:10, FUN = function(x) { 
                paste(
                    sample(c(letters, LETTERS), size = sample(5:20, 1), replace = TRUE), 
                    collapse = "") 
            }, 
            character(1) ),
        stringsAsFactors = FALSE
    )
    
    
    expect_silent(h5write(obj = test_frame, file = h5file, name = 'data_frame'))
    
    expect_silent(back_frame <- h5read(file = h5file, name = "data_frame") )
    expect_equivalent(back_frame$integer, test_frame$integer)
    expect_equivalent(back_frame$words, test_frame$words)
    
})

test_that("character vectors survive a round trip", {
    
    words <- vapply(1:10, FUN = function(x) { 
            paste(
                sample(c(letters, LETTERS), size = sample(5:20, 1), replace = TRUE), 
                collapse = "") 
        }, 
        character(1) )

    expect_silent(h5write(obj = words, file = h5file, name = 'char'))
    
    expect_silent(back_words <- h5read(file = h5file, name = "char") )
    expect_equivalent(words, back_words)
    
})

test_that("UTF-8 strings are preserved", {
  
  input <- "α ≤ 0.1"
  expect_silent(h5write(obj = input, file = h5file, name = 'utf8'))
  expect_silent(output <- h5read(file = h5file, name = "utf8"))
  expect_equivalent(input, output)
  expect_equal(Encoding(output), "UTF-8")
})

test_that("Complex numbers are writen to a compound datatype", {

  mat <- matrix(as.complex(1:9), ncol = 3)
  expect_silent(h5write(obj = mat, file = h5file, name = 'complex'))
  
  ## we don't expect to get back a complex number at the moment
  ## We should get a list of length 2 containing the real and imaginary parts
  expect_silent(res <- h5read(file = h5file, name = 'complex', 
                       compoundAsDataFrame = FALSE))
  expect_is(res, 'list')
  expect_named(res, expected = c('r','i'))
  expect_identical(res$r, Re(mat))
  expect_identical(res$i, Im(mat))
})
