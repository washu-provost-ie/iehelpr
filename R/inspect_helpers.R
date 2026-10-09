### HELPER ###
get_levels_string <- function(x, exclude_num = TRUE, max_levels = 10) {
  if(is.list(x)) {
    return("<NA>")
  }

  if(exclude_num && is.numeric(x)) {
    return("<NA>")
  }

  if(dplyr::n_distinct(x) > max_levels) {
    return("<NA>")
  }

  lvls <- helpr::ifelse1(is.factor(x), levels(x), sort(unique(x)))

  lvls_str <- lvls |>
    purrr::map_chr(.f = deparse1) |>
    paste(collapse = ", ")

  return(glue::glue("{length(lvls)}: [{lvls_str}]"))
}

### HELPER ###
get_values_string <- function(x, n_values = 4) {
  if(lubridate::is.POSIXct(x) || lubridate::is.POSIXlt(x) || is.factor(x)) x <- as.character(x)

  x |>
    head(n = n_values) |>
    purrr::map_chr(.f = deparse1) |>
    paste(collapse = ", ") |>
    stringr::str_replace(pattern = "NA_.+_", replacement = "NA")
}


#' Get a Useful Summary of a Data Frame's Fields
#'
#' Similar to [dplyr::glimpse()], `glimpse2()` returns information (name, type, levels, sample values)
#' about every field in the data frame. `inspect()` is a wrapper for `glimpse2()` that automatically opens
#' the output in the viewer rather than saving it in the environment. This facilitates the typical function
#' of `glimpse2()`, which is to visually inspect column characteristics during iterative data-cleaning
#' processes.
#'
#' @param x A data frame
#' @param exclude_num_levels logical flag, should the `levels` column "exclude" numeric fields? If `TRUE`, then
#' the `levels` value will be `NA` for all of the numeric columns in `x`.
#' @param max_levels an integer, the maximum number of category levels to display. Levels will only be displayed
#' for columns with a number of distinct values less than or equal to this number. The `levels` value will be `NA`
#' for columns in `x` that have more distinct values than this.
#' @param n_values an integer, the number of values to display in the `values` column.
#'
#' @returns A data frame - a summary of `x` in which every row describes a column of `x`
#' @export
glimpse2 <- function(x, exclude_num_levels = TRUE, max_levels = 10, n_values = 4) {
  out <- tibble::tibble(
    name = names(x),
    type = purrr::map_chr(x, .f = \(x) rlang::as_label(x)),
    levels = purrr::map_chr(x, .f = \(x) {
      get_levels_string(x, exclude_num = exclude_num_levels, max_levels = max_levels)
    }),
    values = purrr::map_chr(x, .f = \(x) get_values_string(x, n_values = n_values))
  ) |>
    dplyr::mutate(dplyr::across(type:values, .fns = unname))

  return(out)
}

#' @rdname glimpse2
#' @export
inspect <- function(x, exclude_num_levels = TRUE, max_levels = 10, n_values = 4, data_name = rlang::caller_arg(x)) {
  ## inspect will only have an effect if you are working interactively. Otherwise it will just invisibly return nothing
  if(interative()) {
   out_name <- glue::glue("{data_name}_inspect")
   out <- glimpse2(x, exclude_num_levels = exclude_num_levels, max_levels = max_levels, n_values = n_values)
   View(out, title = out_name)
  }

  invisible(NULL)
}

tst <- tibble::tibble(
  a = c("foo", "bar", "baz", "blah", "foo"),
  b = c(1, 10, 11, 4, 7),
  c = c(TRUE, TRUE, FALSE, FALSE, TRUE),
  d = list(c("a", "b"), "c", c("xx", "zz"), 1:3, TRUE)
)





