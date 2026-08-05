#' Set Working Directory to Wherever the Current Script is
#'
#' @returns no return, changes working directory
#' @export
go_cur_file <- function() {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

#' Return normalized path
#'
#' Wrapper for [file.path()] that returns a normalized path
#'
#' @param ... path components
#'
#' @returns full absolute path
#' @export
file_path <- function(...) {
  file.path(...) |>
    normalizePath(winslash = "/", mustWork = FALSE)
}

#' Convenience Functions to Facilitate Reading Data
#'
#' `read_filepath()` detects file extensions and reads the most common types of rectangular data
#' files (csv, excel, and rds). `read_match()` does the same and also allows you to specify the file with
#' a pattern that matches exactly one filename in `dir_path`. This allows you to avoid writing or copying
#' long filenames when reading data.
#'
#' @param file path to a file
#' @param ... arguments to pass along to the underlying read function (the function used will
#' depend upon the file extension).
#' @param filename a pattern that matches exactly one filename in `dir_path`
#' @param dir_path path to the directory containing the data file. "." by default
#'
#' @returns The result of reading the file, most likely a tibble
#' @export
read_filepath <- function(file, ...) {
  dots <- rlang::list2(...)

  extension <- stringr::str_extract(file, pattern = "\\.\\w+$") |>
    tolower()

  read_fn <- switch(extension,
                    ".csv" = readr::read_csv,
                    ".xls" = readxl::read_xls,
                    ".xlsx" = readxl::read_xlsx,
                    ".rds" = readRDS)

  rlang::exec(read_fn, file, !!!dots)
}

#' @rdname read_filepath
#' @export
read_match <- function(filename, dir_path = ".", ...) {
  path <- hook_path(pattern = filename, dir_path = dir_path)
  read_filepath(file = path, ...)
}


#' Return the Filename that Matches a Pattern
#'
#' `hook_file()` is a helper that "hooks" a single filename based on a regex pattern (see [hook()]). Use
#' inside of read functions to allow those functions to accept patterns rather than full filenames. `hook_path()`
#' is just a wrapper that returns the full path.
#'
#' @param pattern A pattern that matches exactly one filename in dir_path
#' @param dir_path path giving the directory in which to search for `pattern`. Defaults to working directory
#' @param return_full_path TRUE or FALSE - should the full path be returned? If FALSE, just the filename is returned
#'
#' @returns A filename, or if `return_full_path` is TRUE, then a full path
#' @export
hook_file <- function(pattern, dir_path = ".", return_full_path = FALSE) {
  files <- list.files(dir_path) |>
    stringr::str_subset(pattern = "^~", negate = TRUE)

  path <- hook(pattern, table = files)

  if(return_full_path) path <- file_path(dir_path, path)

  return(path)
}

#' @rdname hook_file
#' @export
hook_path <- function(pattern, dir_path = ".") {
  hook_file(pattern = pattern, dir_path = dir_path, return_full_path = TRUE)
}
