#' Recode and Replace Using Regex Patterns
#'
#' Wrappers for [dplyr::recode_values()] and [dplyr::replace_values()] that allow the user to use unique regex patterns
#' (rather than exact values) to specify which values to be replaced or recoded. Each pattern must uniquely match
#' exactly one value in the input vector (see [hook()]). As with the original dplyr versions, the difference is that
#' `recode_hook()` intends to match and update every value in `x` (unmatched values become `NA` by default). `replace_hook()`,
#' on the other hand, intends to update some specified values, but otherwise retains the original values of `x` that
#' aren't reference in `from` or `...`.
#'
#' @inheritParams dplyr::recode_values
#' @param ... two sided formulas where `lhs` determines the values to be recoded and `rhs` provides the replacement
#' value. The `lhs` can be a character vector of any size, and each element must be a regex pattern that uniquely
#' matches a single value in `x`. `...` is mutually exclusive with `from` and `to`
#' @param enforce_match For `recode_hook, `TRUE` or `FALSE`. Should the function throw an error if there is a
#' pattern in `...` or `from` that doesn't match any values in `x`. By default this is `FALSE` - the function
#' will inform the user of the unmatched pattern but still return the recoded version of `x`.
#'
#' @returns A vector - `x` with specified updates
#' @export
recode_hook <- function(x, ..., from = NULL, to = NULL, default = NULL, unmatched = "default", ptype = NULL,
                        enforce_match = FALSE) {
  dots <- rlang::list2(...)

  if(!rlang::is_empty(dots) && !is.null(from)) {
    cli::cli_abort("Can't supply both `from` and `...`")
  }

  if(is.null(from)) {
    from <- purrr::map(dots, .f = \(x) x |> rlang::f_lhs() |> rlang::eval_tidy())
    to <- purrr::map(dots, .f = \(x) x |> rlang::f_rhs() |> rlang::eval_tidy())
  }

  empty <- ifelse(enforce_match, "error", "return_pattern")
  table <- unique(x)

  from <- purrr::map(from, .f = \(patterns) {
    hook_each(patterns, table = table, empty = empty)
  })

  dplyr::recode_values(x, from = from, to = to, default = default, unmatched = unmatched, ptype = ptype)
}

#' @rdname recode_hook
#' @export
replace_hook <- function(x, ..., from = NULL, to = NULL) {
  recode_hook(x, ..., from = from, to = to, default = x)
}


#' Replace all Values that Match Regex Patterns
#'
#' Replace all items that match a pattern. Unlike `replace_hook()`, a single pattern does not need to match
#' a single value of `x`. A pattern can match any number of values in `x`, and all matching values will be replaced
#' with the same corresponding value.
#'
#' @param x a character vector
#' @param ... two sided formulas where `lhs` determines the values to be recoded and `rhs` provides the replacement
#' value. The `lhs` must be a single pattern, and it can match any number of values in `x`. For each pattern, all
#' matching values in `x` will be replaced with the `rhs` value `...` is mutually exclusive with `from` and `to`
#' @param from regex pattern specifying values of `x` to replace
#' @param to Replacement values
#'
#' @returns A character vector
#' @export
replace_matches <- function(x, ..., from = NULL, to = NULL) {
  dots <- rlang::list2(...)

  if(!rlang::is_empty(dots) && !is.null(from)) {
    cli::cli_abort("Can't supply both `from` and `...`")
  }

  if(is.null(from)) {
    from <- purrr::map(dots, .f = \(x) x |> rlang::f_lhs() |> rlang::eval_tidy())
    to <- purrr::map(dots, .f = \(x) x |> rlang::f_rhs() |> rlang::eval_tidy())
  }

  table <- unique(x)
  # unlike the "hook" functions, here we want to collect every value that matches the pattern
  from <- purrr::map(from, .f = \(pattern) stringr::str_subset(table, pattern = pattern))

  ## Check that no values got matched by multiple patterns
  from_vals <- purrr::list_c(from)
  if(any(duplicated(from_vals))) {
    dup_vals <- from_vals[duplicated(from_vals)]
    cli::cli_abort("The value{?s} {.val {dup_vals}} matched multiple patterns")
  }

  dplyr::replace_values(x, from = from, to = to)
}


#' Perform Multiple String Replacements at Once
#'
#' `str_replace_multi()` provides a compact way to perform multiple string replacements on the same
#' vector (as opposed to calling [stringr::str_replace_all()] multiple times to make these replacements).
#' Additionally, this function always detects matches on the original input vector, preventing accidental
#' matches that could occur with multiple calls to `str_replace_all`, where later calls are detecting
#' matches on updated versions of the vector.
#'
#' @param string A character vector
#' @param ... Formulas where the `lhs` is a regex pattern, and `rhs` is the replacement value.
#'
#' @returns A character vector
#' @export
str_replace_multi <- function(string, ...) {
  dots <- rlang::list2(...)
  lhs <- purrr::map_chr(dots, .f = \(x) x |> rlang::f_lhs() |> rlang::eval_tidy())
  rhs <- purrr::map_chr(dots, .f = \(x) x |> rlang::f_rhs() |> rlang::eval_tidy())

  # Keep track of whether str_detect was TRUE for the ORIGINAL vector
  update_list <- purrr::map(lhs, .f = \(pattern) stringr::str_detect(string, pattern = pattern))

  for(i in seq_along(dots)) {
    is_update <- update_list[[i]]
    pattern <- lhs[[i]]
    replacement <- rhs[[i]]

    # updates will only be made if the pattern matched the orignal vector.
    string[is_update] <- stringr::str_replace(string[is_update], pattern = pattern, replacement = replacement)
  }

  return(string)
}


#' Joins with Easy Relocation
#'
#' [dplyr::left_join()] and [dplyr::inner_join()] add all columns from `y` to the end of the output data frame.
#' `left_join2()` and `inner_join2()` allow you to use `.before` and `.after` arguments to control where the `y`
#' columns get placed in the output. This is equivalent to running `relocate()` after the join functions to
#' relocate every column that was originally in `y` but saves the hassle of making the separate `relocate()` call
#' and specifying all of the columns.
#'
#' NOTE: these functions will relocate every single column of `.y`. If you need more specificity than that, use the
#' `dplyr` join functions + `relocate()` to relocate only specific columns
#'
#' @inheritParams dplyr::left_join
#' @inheritParams dplyr::relocate
#'
#' @returns A tibble
#' @export
join2 <- function(x, y, by = NULL, .before = NULL, .after = NULL, copy = FALSE, suffix = c(".x", ".y"),
                  ..., keep = NULL, join_fn = dplyr::left_join) {
  out <- join_fn(x, y, by = {{by}}, copy = copy, suffix = suffix, ..., keep = keep)

  num_old_cols <- ncol(x)
  new_cols <- (num_old_cols + 1):ncol(out)

  out <- dplyr::relocate(out, all_of(new_cols), .before = {{.before}}, .after = {{.after}})
  return(out)
}

#' @rdname join2
#' @export
left_join2 <- function(x, y, by = NULL, .before = NULL, .after = NULL, copy = FALSE, suffix = c(".x", ".y"),
                       ..., keep = NULL, join_fn = dplyr::left_join) {
  join2(x, y, by = {{by}}, .before = {{.before}}, .after = {{.after}}, copy = copy, suffix = suffix,
        ..., keep = keep, join_fn = dplyr::left_join)
}

#' @rdname join2
#' @export
inner_join2 <- function(x, y, by = NULL, .before = NULL, .after = NULL, copy = FALSE, suffix = c(".x", ".y"),
                       ..., keep = NULL, join_fn = dplyr::left_join) {
  join2(x, y, by = {{by}}, .before = {{.before}}, .after = {{.after}}, copy = copy, suffix = suffix,
        ..., keep = keep, join_fn = dplyr::inner_join)
}
