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
hook_path <- function(pattern, dir_path) {
  hook_file(pattern = pattern, dir_path = dir_path, return_full_path = TRUE)
}
