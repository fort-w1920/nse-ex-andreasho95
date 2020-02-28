source("nse_sol.R")

context("Function get2")

test_that("get2 works correctly", {
  test_env <- new.env()
  test_env$test_var <- 2
  expect_equal(2, get2("test_var", test_env))
  expect_error(get2("test_var", globalenv()), "not found")
  expect_error(get2("test_var", "not an env"), "not 'character'")
  expect_error(get2(mtcars, globalenv()), "not 'data.frame'")
})


