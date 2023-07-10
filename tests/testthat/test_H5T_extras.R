library(rhdf5)

tid <- H5Tenum_create()
H5Tenum_insert(tid, name = "TRUE",  value = 1L)
H5Tenum_insert(tid, name = "FALSE", value = 0L)

test_that("enum details are extracted", {
    
  expect_identical(h5getEnumNames( tid ), c("TRUE", "FALSE"))
  expect_identical(h5getEnumValues( tid ), c(1L, 0L))
  
})



