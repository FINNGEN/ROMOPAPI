library(testthat)
library(ROMOPAPI)

context("API functions")

test_that("create_api returns a plumber router", {
  # Skip if plumber is not available
  skip_if_not_installed("plumber")
  
  # Create the API
  api <- create_api()
  
  # Check that it's a plumber router
  expect_true("Plumber" %in% class(api))
})

# Test the endpoints by mocking requests
test_that("process-ids endpoint validates IDs correctly", {
  # Skip if plumber is not available
  skip_if_not_installed("plumber")
  
  # Create the API
  api <- create_api()
  
  # Get the process-ids endpoint function
  endpoints <- api$routes
  process_ids_endpoint <- NULL
  
  for (endpoint in endpoints) {
    if (endpoint$path == "/process-ids" && endpoint$verbs$get) {
      process_ids_endpoint <- endpoint$handler
      break
    }
  }
  
  # Check that we found the endpoint
  expect_false(is.null(process_ids_endpoint))
  
  # Test with valid IDs
  result <- process_ids_endpoint(ids = "1,2,3")
  expect_equal(result$status, "success")
  expect_equal(result$ids, c(1L, 2L, 3L))
  
  # Test with invalid IDs
  result <- process_ids_endpoint(ids = "1.5,2.5")
  expect_equal(result$status, "error")
  expect_true(grepl("must contain only integer values", result$message))
})

test_that("batch-process endpoint validates IDs correctly", {
  # Skip if plumber is not available
  skip_if_not_installed("plumber")
  
  # Create the API
  api <- create_api()
  
  # Get the batch-process endpoint function
  endpoints <- api$routes
  batch_process_endpoint <- NULL
  
  for (endpoint in endpoints) {
    if (endpoint$path == "/batch-process" && endpoint$verbs$post) {
      batch_process_endpoint <- endpoint$handler
      break
    }
  }
  
  # Check that we found the endpoint
  expect_false(is.null(batch_process_endpoint))
  
  # Create a mock request with valid IDs
  req <- list(body = list(ids = c(1, 2, 3)))
  result <- batch_process_endpoint(req)
  expect_equal(result$status, "success")
  expect_equal(result$ids, c(1L, 2L, 3L))
  
  # Create a mock request with invalid IDs
  req <- list(body = list(ids = c(1.5, 2.5)))
  result <- batch_process_endpoint(req)
  expect_equal(result$status, "error")
  expect_true(grepl("must contain only integer values", result$message))
  
  # Create a mock request with missing IDs
  req <- list(body = list())
  result <- batch_process_endpoint(req)
  expect_equal(result$status, "error")
  expect_true(grepl("must contain an 'ids' field", result$message))
}) 