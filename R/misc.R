#' Check Object Length
#'
#' Convenience function to check length (e.g., is_length(x, 2) vs identical(length(x), 2L)).
#'
#' @param x An r object
#' @param len the target length to check for
#'
#' @returns TRUE or FALSE
#' @export
is_length <- function(x, len) {
  identical(length(x), as.integer(len))
}


#' Object Length Checks
#'
#' Convenience functions to check important length thresholds, complementing [rlang::is_empty()]
#' * [rlang::is_empty()] - is the length 0?
#' * `not_empty()` - is the length > 0
#' * `is_single()` - is the length exactly 1?
#' * `is_multiple()` - is the length > 1?
#'
#' @param x An R object
#'
#' @returns TRUE or FALSE
#' @name length_checks
NULL

#' @rdname length_checks
#' @export
not_empty <- function(x) {
  length(x) > 0
}

#' @rdname length_checks
#' @export
is_single <- function(x) {
  length(x) == 1
}

#' @rdname length_checks
#' @export
is_multiple <- function(x) {
  length(x) >= 2
}


#' Is There a Set Difference?
#'
#' Test whether there is a setdiff. Returns `TRUE` if `x` has any items that are not in `y`
#'
#' @param x, y vectors
#'
#' @returns TRUE or FALSE
#' @export
is_setdiff <- function(x, y) {
  not_empty(setdiff(x, y))
}


#' Check Whether an Object is "Scalar"
#'
#' Returns `TRUE` if `x` is length 1 AND is not a list. This prevents something like a 1-column
#' data frame from being considered scalar (on the other hand, a 1-column data frame will return
#' `TRUE` for `is_single()`)
#'
#' @param x An R object
#'
#' @returns TRUE or FALSE
#' @export
is_scalar <- function(x) {
  !rlang::is_list(x) && is_single(x)
}


#' Check Whether an Object is "Flat"
#'
#' A flat object is a vector or a list whose elements are all scalar objects and/or individual formulas.
#' (although formulas technically have length 2 or 3 this check treats a formula as a single object)
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
is_flat <- function(x) {
  for(el in x) {
    if(!is_scalar(el) && !rlang::is_formula(el)) {
      return(FALSE)
    }
  }

  return(TRUE)
}


#' Convert A Vector or Flat List
#'
#' Given a vector or a flat list, this function converts to a vector of a given ptype. This is especially
#' useful for combining lists of length-1 elements into a vector. If no ptype provided, the conversion will
#' be to the common ptype.
#'
#' The following variants enforce conversion to specific types of vectors:
#' * as_chr() to character(0)
#' * as_dbl() to double(0)
#' * as_int() to integer(0)
#' * as_lgl() to logical(0)
#' * as_fct() to factor(0)
#'
#' @param x A vector or flat list
#' @param .ptype optional, the target ptype for the conversion
#' @param levels optional character vector specifying factor levels. If not provided, levels will be all
#' values that appear in `x`
#'
#' @returns A vector
#' @export
as_vec <- function(x, .ptype = NULL) {
  if(!is_flat(x)) {
    cli::cli_abort("`x` must be flat")
  }

  vctrs::vec_c(!!!x, .ptype = .ptype)
}

#' @rdname as_vec
#' @export
as_chr <- \(x) as_vec(x, .ptype = character(0))

#' @rdname as_vec
#' @export
as_dbl <- \(x) as_vec(x, .ptype = double(0))

#' @rdname as_vec
#' @export
as_int <- \(x) as_vec(x, .ptype = integer(0))

#' @rdname as_vec
#' @export
as_lgl <- \(x) as_vec(x, .ptype = logical(0))

#' @rdname as_vec
#' @export
as_fct <- function(x, levels = NULL) {
  x <- as_chr(x)

  if(is.null(levels)) {
    return(factor(x))
  }

  if(is_setdiff(x, levels)) {
    cli::cli_abort("{.arg levels} must contain all values of {.arg x}")
  }

  return(factor(x, levels = levels))
}




