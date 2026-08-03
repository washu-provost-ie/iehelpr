#' Extract Year
#'
#' Given any input vector that consistently contains 2-digit and/or 4-digit numbers, this function extracts into
#' a vector of "years". 2-digit numbers are automatically prepenced with "20".
#'
#' @param sem A vector containing semesters in some format
#'
#' @returns An integer vector of 4-digit years
#' @export
extract_year <- function(x, is_2000 = TRUE) {
  # grab the numeric component
  year <- stringr::str_extract(x, pattern = "\\d+")

  digits <- purrr::map_int(year, .f = stringr::str_length) |>
    unique()

  if(digits %not% 2 && digits %not% 4) {
    cli::cli_abort("{.arg x} must contain either all 2-digit years or all 4-digit years")
  }

  if(is_2000 && digits %is% 4 && !(all(stringr::str_detect(year, pattern = "^20")))) {
    cli::cli_abort("if {.arg is_2000} is {.val {TRUE}}, all years must start with \"20\"")
  }

  if(digits %is% 2) {
    if(!is_2000) {
      cli::cli_abort("if {.arg is_2000} is {.val {FALSE}}, {.arg x} must contain all 4-digit years")
    }

    year <- paste0("20", year)
  }

  return(as.integer(year))
}



#' More Lenient version of `identical()`
#'
#' `identical()` is very strict, requiring all classes and attributes to the exactly the same. `equal()` is a
#' slightly more lenient version that first attempts the two objects to a common [vctrs] ptype before testing
#' whether they are identical. `%is%` is just a convenient inline version of `equal()` and `%not%` a convenient
#' negation of `%is%``
#'
#' @param x,y R objects
#'
#' @returns TRUE or FALSE
#' @export
equal <- function(x, y) {
  ptype <- rlang::catch_cnd(vctrs::vec_ptype2(x, y), classes = "vctrs_error")

  if(rlang::is_error(ptype)) {
    return(FALSE)
  }

  c(x, y) %<-% vctrs::vec_cast_common(x, y)
  identical(x, y)
}

#' @rdname equal
#' @export
`%is%` <- \(x, y) equal(x, y)

#' @rdname equal
#' @export
`%not%` <- \(x, y) !equal(x, y)



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
#' * `is_scalar()` - is the length exactly 1?
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
is_scalar <- function(x) {
  length(x) == 1
}

#' @rdname length_checks
#' @export
is_multiple <- function(x) {
  length(x) >= 2
}


#' Is an Object Scalar or a Formula
#'
#' This test expands the definition of "scalar" to include a single formula. Even though formulas are
#' technically length 2 or 3, it often makes sense to think of them as single element. `is_scalarish()`
#' facilitates this by testing whether something is either a "real scalar" or a formula.
#'
#' @param x An object
#'
#' @returns TRUE or FALSE
#' @export
is_scalarish <- function(x) {
  is_scalar(x) || rlang::is_formula(x)
}

#' Check Whether an Object is "Flat"
#'
#' A flat object is a vector or a list whose elements are all scalar atomic elements and/or individual formulas
#' (although formulas technically have length 2 or 3 this check treats a formula as a single object)
#'
#' @param x A list or vector
#'
#' @returns TRUE or FALSE
#' @export
is_flat <- function(x) {
  for(el in x) {
    if(!rlang::is_scalar_atomic(el) && !rlang::is_formula(el)) {
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


#' Attempt to Return a Single regex match
#'
#' A variant of [stringr::str_subset()] to use when you want or expect to return a single string that
#' matches `pattern`.
#' This function always throws an error when more than one value of `string` matches `pattern`. Use the
#' `empty` argument to control what happens when there are no matches.
#'
#' @param string A character vector
#' @param pattern A single regex pattern
#' @param empty Controls what happens when no matches occur. By default ("error") an error is thrown.
#' You can also choose to return `pattern` or return `NA`
#'
#' @returns A single string
#' @export
str_subset1 <- function(string, pattern, empty = c("error", "return_pattern", "return_na")) {
  empty <- rlang::arg_match(empty)

  assert_scalar_character(pattern)
  string <- unique(string)

  match <- stringr::str_subset(string, pattern = pattern)

  if(is_scalar(match)) {
    return(match)
  }

  if(is_multiple(match)) {
    cli::cli_abort(c("More than one string in {.arg string} matches {.arg pattern}",
                     "i" = "matching strings are {.val {match}}",
                     "i" = "update the {.arg pattern} {.val {pattern}} to match only one string"))
  }

  if(empty %is% "error") {
    cli::cli_abort(c("No strings in {.arg string} match {.arg pattern}",
                     "i" = "the {.arg pattern} is {.val {pattern}}"))
  }

  if(empty %is% "return_pattern") {
    cli::cli_inform("No matches, the pattern {.val {pattern}} is being returned")
    out <- pattern
  } else {
    cli::cli_inform("No matches, returning {.val {NA}}")
    out <- NA_character_
  }

  return(out)
}


#' Use a Pattern to "hook" the Matching Value
#'
#' Given a pattern and set of candidate values, this function returns the single matching value. This is
#' designed to be helper function that facilitates users' ability to use a (short) regex pattern as a
#' stand-in for a (possibly long) full value (e.g. a filename). It provides safety by ensuring that the
#' pattern "hooks" a single value (presumably the one the user intended the pattern to represent.)
#' This function always throws an error when `pattern` matches more than one value in `vals`. Use the
#' `empty` argument to control what happens when there are no matches.
#'
#' This function is really just a wrapper for [str_subset1()] with the arguments renamed and
#' rearranged to more intuitively match the use case (using a single pattern to retrieve a
#' single candidate value)
#'
#' @param pattern a single regex pattern
#' @param table a vector of candidate values
#' @param empty Controls what happens when no matches occur. By default ("error") an error is thrown.
#' You can also choose to return `pattern` or return `NA`
#'
#' @returns A single string
#' @export
hook <- function(pattern, table, empty = c("error", "return_pattern", "return_na")) {
  str_subset1(table, pattern = pattern, empty = empty)
}


#' Is an Expression or Quosure evaluable?
#'
#' Tests whether an expression or quosure can be evaluated with [rlang::eval_tidy()].
#'
#' @param expr An expression or quosure
#'
#' @returns TRUE or FALSE
#' @export
is_evaluable <- function(expr) {
  res <- try(rlang::eval_tidy(expr), silent = TRUE)
  !inherits(res, "try-error")
}


#' Check Whether a Quosure is Storing a Waiver Object
#'
#' Check whether an expression or quosure evaluates to `waiver()`. This is useful for arguments
#' captured with [rlang::enexpr()] or [rlang::enquo()] to check whether the captured argument
#' was actually [waiver()]
#'
#' @param quo A quosure or expression
#'
#' @returns TRUE or FALSE
#' @export
quo_is_waiver <- function(quo) {
  if(!is_evaluable(quo)) {
    return(FALSE)
  }

  is_waiver(rlang::eval_tidy(quo))
}








