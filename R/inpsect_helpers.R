### HELPER ###
get_levels_string <- function(x, exclude_num = FALSE, max_levels = 10) {
  if(is.list(x)) {
    return("<NA>")
  }

  if(exclude_num && !helpr::is_characterish(x)) {
    return("<NA>")
  }

  if(dplyr::n_distinct(x) > max_levels) {
    return("<NA>")
  }

  lvls <- helpr::ifelse1(is.factor(x), levels(x), sort(unique(x)))

  lvls_str <- lvls |>
    purrr::map_chr(.f = deparse1) |>
    paste(collapse = ", ")

  return(helpr::glue2("{length(lvls)}: [{lvls_str}]"))
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


meta <- function(x, max_levels = 10, exclude_num_levels = FALSE, n_values = 4) {
  out <- tibble::tibble(
    name = names(x),
    type = purrr::map_chr(x, .f = \(x) rlang::as_label(x)),

  )

}
