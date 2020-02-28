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


context("Function assign2")

test_that("assign2 works correctly", {
  test_env <- new.env()
  test_env$test_var <- 2
  assign2("test_var", 3, test_env)
  expect_equal(3, test_env$test_var)
  assign2("test_var", "x", test_env)
  expect_equal("x", test_env$test_var)
  my_new_func <- function(t, value, env) {
    x <- 10
    assign2(t, value, env)
  }
  my_new_func("test_var", 8, test_env)
  expect_equal(8, test_env$test_var)
  expect_error(assign2("test_var", 3, "not an env"), "not 'character'")
  expect_error(assign2(mtcars, 3, globalenv()), "not 'data.frame'")
  assign2("test_var2", "x", test_env)
  expect_equal("x", test_env$test_var2)
})
