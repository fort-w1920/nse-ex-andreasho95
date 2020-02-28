
get2 <- function(x, envir) {
  checkmate::assert_character(x, len = 1, all.missing = FALSE, null.ok = FALSE)
  checkmate::assert_environment(envir)

  object_name <- as.name(x)
  # Try-catch is only used in this example to demonstrate the possibility with the envnames package
  # for "later me" :-)
  object <- tryCatch({
    eval(object_name, envir = envir)
  }, error = function(err) {
    # By using envnames package we could give a more detailed error message
    # envname <- envnames::environment_name(envir)
    # err_msg <- paste0(
    #   "Variable `", deparse(substitute(x, env = rlang::env_parent())),
    #   "` could not be found in environment `", envname, "`"
    # )
    # err$message <- err_msg
    stop(err)
  })
  object
}




assign2 <- function(x, value, env) {
  checkmate::assert_character(x, len = 1, all.missing = FALSE, null.ok = FALSE)
  checkmate::assert_environment(env)
  eval(substitute(x <- value, list(x = as.name(x), value = value)), envir  = env)
}

