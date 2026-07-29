#' Assert Scalar Types
#'
#' @param x An object
#' @param x_arg string to label `x` in the error message
#'
#' @returns returns TRUE or throws an error
#' @export
assert_scalar <- function(x, x_arg = rlang::caller_arg(x)) {
  if(!is_scalar(x)) {
    cli::cli_abort("{.arg {x_arg}} must have length 1")
  }

  return(TRUE)
}

#' @rdname assert_scalar
#' @export
assert_scalarish <- function(x, x_arg = rlang::caller_arg(x)) {
  if(!is_scalarish(x)) {
    cli::cli_abort("{.arg {x_arg}} must have length 1")
  }

  return(TRUE)
}

#' @rdname assert_scalar
#' @export
assert_scalar_character <- function(x, x_arg = rlang::caller_arg(x)) {
  if(!rlang::is_scalar_character(x)) {
    cli::cli_abort("{.arg {x_arg}} must be a single string")
  }

  return(TRUE)
}

