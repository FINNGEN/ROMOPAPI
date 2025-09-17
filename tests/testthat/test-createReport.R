test_that("createReport works with basic parameters", {
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    result <- createReport(
      conceptId = 317009,
      CDMdbHandler = CDMdbHandler
    )
  )

  # Check that result is a character string (file path)
  expect_type(result, "character")
  expect_length(result, 1)

  # Check that the file exists
  expect_true(file.exists(result))

  # Check that it's an HTML file
  expect_equal(tools::file_ext(result), "html")

  # Check that the file is not empty
  expect_gt(file.size(result), 0)

  # Clean up the temporary file
  unlink(result)
})

test_that("createReport works with all parameters", {
  CDMdbHandler <- HadesExtras_createCDMdbHandlerFromList(test_cohortTableHandlerConfig, loadConnectionChecksLevel = "basicChecks")
  withr::defer({
    CDMdbHandler <- NULL
    gc()
  })

  suppressWarnings(
    result <- createReport(
      conceptId = 317009,
      CDMdbHandler = CDMdbHandler,
      showsMappings = TRUE,
      pruneLevels = c(2),
      pruneClass = "Ingredient"
    )
  )

  # Check that result is a character string (file path)
  expect_type(result, "character")
  expect_length(result, 1)

  # Check that the file exists
  expect_true(file.exists(result))

  # Check that it's an HTML file
  expect_equal(tools::file_ext(result), "html")

  # Check that the file is not empty
  expect_gt(file.size(result), 0)

  # Clean up the temporary file
  unlink(result)
})

