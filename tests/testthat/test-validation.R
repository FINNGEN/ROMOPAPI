library(testthat)
library(ROMOPAPI)

context("Validation functions")

test_that("toAccept validates integer vectors correctly", {
  # Valid integer vectors should pass
  expect_equal(toAccept(c(1L, 2L, 3L), "test"), c(1L, 2L, 3L))
  expect_equal(toAccept(1L, "test"), 1L)
  
  # Numeric values that are integers should be converted to integers
  expect_equal(toAccept(c(1, 2, 3), "test"), c(1L, 2L, 3L))
  expect_equal(toAccept(1, "test"), 1L)
  
  # Non-integer values should throw an error
  expect_error(toAccept(c(1.5, 2.5), "test"), "must contain only integer values")
  expect_error(toAccept(1.5, "test"), "must contain only integer values")
  
  # Non-vector values should throw an error
  expect_error(toAccept(list(1, 2, 3), "test"), "must be a vector")
  
  # NULL should throw an error by default
  expect_error(toAccept(NULL, "test"), "cannot be NULL")
  
  # NULL should be allowed when allowNull = TRUE
  expect_null(toAccept(NULL, "test", allowNull = TRUE))
  
  # NA values should throw an error by default
  expect_error(toAccept(c(1L, NA_integer_), "test"), "cannot contain NA values")
  
  # NA values should be allowed when allowNA = TRUE
  expect_equal(toAccept(c(1L, NA_integer_), "test", allowNA = TRUE), c(1L, NA_integer_))
}) 