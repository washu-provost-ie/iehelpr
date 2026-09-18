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

### HELPER
is_user_root <- function(path) {
  identical(file_path(path), file_path(Sys.getenv("USERPROFILE")))
}

### HELPER
is_analysis_root <- function(path = ".") {
  subdirs <- dir(path)
  analysis_dirs <- c("01 Raw Data", "02 Cleaning Scripts", "03 Cleaned Data", "04 Analysis")

  is_root <- all(analysis_dirs %in% subdirs)
  return(is_root)
}

### HELPER
get_path_analysis_root <- function(path = ".") {
  path <- file_path(path)

  if(is_user_root(path)) {
    cli::cli_abort("You have reached the system root. You don't appear to be inside of an analysis")
  }

  if(is_analysis_root(path)) {
    return(path)
  }

  parent_path <- file_path(path, "..")
  return(get_path_analysis_root(parent_path))
}


#' Work with "Raw Data" and "Cleaned Data" Inside an Analysis Folder
#'
#' From anywhere inside of an analysis folder, these functions facilitate working with the
#' "01 Raw Data" and "03 Cleaned Data" subdirectories. `peek_raw()` and `peek_cleaned()` show
#' the available filenames within these directories. `read_raw()` and `read_cleaned()` allow
#' you to read an available file from these directories, and allow you to specify file by a
#' uniquely identifying regex pattern rather than a full filename (see [read_match()]).
#' `write_raw()` writes a tibble to "01 Raw Data" (as a csv by default).
#' `write_cleaned()` writes a tibble to "03 Cleaned Data" in both csv and rds formats.
#'
#' Be careful when using `write_raw()` - you should only use when pulling in a data source and
#' immediately saving the result. Data with any further processing should not be saved in "01 Raw Data"
#'
#' NOTE: you do not have to be in the top-level of an analysis directory to use these functions.
#' You can be ANYWHERE within an analysis folder and the function will correctly find the
#' "01 Raw Data" or "03 Cleaned Data" subdirectory. If your working directory is not within an
#' analysis folder, all of these functions should throw an error indicating that you've reached
#' the root of your system.
#'
#' @param file for "read_" functions, a pattern that uniquely matches one filename in "01 Raw Data" or "03 Cleaned Data".
#' for "write_" functions, then exact name of the output file (including file extension is optional)
#' @param ... arguments to pass to [readr::read_csv()] or [readr::write_csv()]
#' @param x a tibble
#' @param is_rds TRUE or FALSE - should the ".rds" version be read? The recommended (and default)
#' values is TRUE. If FALSE, the ".csv" version of the cleaned data will be read
#' @param na for "write_" functions, the string used for missing values. By default this is "" (empty cell),
#' unlike the default for `write_csv()`, which is the string "NA"
#'
#' @name analysis_directory_helpers
NULL

#' @rdname analysis_directory_helpers
#' @export
peek_raw <- function() {
  path <- file_path(get_path_analysis_root(), "01 Raw Data")
  dir(path)
}

#' @rdname analysis_directory_helpers
#' @export
peek_cleaned <- function() {
  path <- file_path(get_path_analysis_root(), "03 Cleaned Data")
  dir(path)
}

#' @rdname analysis_directory_helpers
#' @export
read_raw <- function(file, ...) {
  dir_path <- file_path(get_path_analysis_root(), "01 Raw Data")
  read_match(file, ..., dir_path = dir_path)
}

#' @rdname analysis_directory_helpers
#' @export
read_cleaned <- function(file, ..., is_rds = TRUE) {
  dir_path <- file_path(get_path_analysis_root(), "03 Cleaned Data")
  if(is_rds) dir_path <- file_path(dir_path, "rds Versions")
  read_match(file, ..., dir_path = dir_path)
}

#' @rdname analysis_directory_helpers
#' @export
write_raw <- function(x, file, na = "", ...) {
  dir_path <- file_path(get_path_analysis_root(), "01 Raw Data")

  if(stringr::str_detect(file, pattern = "xlsx")) {
    # if file has explicit excel extension we can save it as excel
    path <- file_path(dir_path, file)
    openxlsx::write.xlsx(x, file = path, ...)
  } else {
    # but by default, we will save as a csv
    path <- file_path(dir_path, ensure_csv(file))
    readr::write_csv(x, file = path, na = na,  ...)
  }
}

#' @rdname analysis_directory_helpers
#' @export
write_cleaned <- function(x, file, na = "", ...) {
  dir_path <- file_path(get_path_analysis_root(), "03 Cleaned Data")

  file <- stringr::str_remove(file, pattern = "\\.csv$|\\.xlsx$\\.[Rd][Dd][Ss]$")
  if(!stringr::str_detect(file, pattern = "[Cc]lean")) file <- paste0("cleaned_", file)

  csv_file <- glue::glue("{file}.csv")
  csv_path <- file_path(dir_path, csv_file)
  readr::write_csv(x, file = csv_path, na = na, ...)

  rds_file <- glue::glue("{file}.rds")
  rds_path <- file_path(dir_path, "rds Versions", rds_file)
  saveRDS(x, file = rds_path)
}

#' Easily Extract Pieces of a Filename
#'
#' `file_extension()` extracts the file extension (including the leading ".") from a filename, and
#' `file_basename()` removes the extension
#'
#' @param x A filename or vector of filenames
#'
#' @returns A string - the requested piece of `filename`
#' @export
file_extension <- function(file) {
  stringr::str_extract(file, pattern = "\\.\\w+$")
}

#' @rdname file_extension
#' @export
file_basename <- function(file) {
  stringr::str_remove(file, pattern = "\\.\\w+$")
}

#' Ensure Filename has a Given Extension
#'
#' Checks if a filename has an expected extension and if not, adds it. Other `ensure_` functions
#' are wrappers that check for a specific extention (e.g., `ensure_r()` checks for a ".R" extension)
#'
#' @param file A filename
#' @param ext A file extension (will work with our without the leading ".")
#'
#' @returns a filename as a string
#' @export
ensure_extension <- function(file, ext) {
  # if they enter ext without the leading period, add it in
  if(!stringr::str_detect(ext, pattern = "^\\.")) ext <- paste0(".", ext)
  if(!stringr::str_detect(file, pattern = paste0(ext, "$"))) file <- paste0(file, ext)
  return(file)
}

#' @rdname ensure_extension
#' @export
ensure_r <- \(file) ensure_extension(file, ext = ".R")

#' @rdname ensure_extension
#' @export
ensure_rmd <- \(file) ensure_extension(file, ext = ".Rmd")

#' @rdname ensure_extension
#' @export
ensure_rds <- \(file) ensure_extension(file, ext = ".rds")

#' @rdname ensure_extension
#' @export
ensure_csv <- \(file) ensure_extension(file, ext = ".csv")


# HELPER
get_path_template <- function(template_name) {
  template_dir <- Sys.getenv("TEMPLATE_DIR")
  hook_path(template_name, dir_path = template_dir)
}

#' Work with Templates
#'
#' See and worth with templates in "00 IE/Resources/R Resources/Templates". `peek_templates()`
#' lets you see names of available templates, `copy_template()` creates a new file that copies
#' the template, and `edit_template()` opens the template in its source location for editing.
#'
#' @param template_name a pattern that uniquely matches the name of one template
#' @param to the path to which the template should be copied
#'
#' @name template_helpers
NULL

#' @rdname template_helpers
#' @export
peek_templates <- function() {
  template_dir <- Sys.getenv("TEMPLATE_DIR")
  dir(template_dir)
}

#' @rdname template_helpers
#' @export
copy_template <- function(template_name, to) {
  template_path <- get_path_template(template_name)
  file.copy(from = template_path, to )
}

#' @rdname template_helpers
#' @export
edit_template <- function(template_name) {
  template_path <- get_path_template(template_name)
  file.open(path)
}


#' Add a script
#'
#' `add_script()` creates a file (like[file.create()]), but it also enables the new file to be created from a template.
#' `add_script_cleaning()`, `add_script_analysis()`, and `add_script_helper()` are wrappers that automatically place
#' the new scripts in the proper "cleaning", "analysis", or "helper script" directories and use default templates. These
#' functions only work from inside an analysis directory.
#'
#' @param path the path of the new file to create
#' @param template optional, the name of a template that the new file should copy. Must
#' be a pattern that uniquely matches one template name
#' @param open TRUE or FALSE - should the new script be opened in R Studio?
#'
#' @export
add_script <- function(path, template = NULL, open = TRUE) {
  if(is.null(template)) {
    file.create(path)
    return(TRUE)
  }

  temp_path <- get_path_template(template_name = template)
  file.copy(from = temp_path, to = path)

  if(open) {
    file.edit(path)
  }

  return(TRUE)
}

#' @rdname add_script
#' @export
add_script_cleaning <- function(name) {
  dir_path <- file_path(get_path_analysis_root(), "02 Cleaning Scripts")
  if(!stringr::str_detect(name, pattern = "^clean")) name <- paste("cleaning", name, sep = "_")
  path <- file_path(dir_path, ensure_rmd(name))
  add_script(path, template = "clean_general")
}

#' @rdname add_script
#' @export
add_script_analysis <- function(name) {
  dir_path <- file_path(get_path_analysis_root(), "04 Analysis", "Analysis Scripts")
  path <- file_path(dir_path, ensure_rmd(name))
  add_script(path, template = "analyze_general")
}


# HELPER
get_path_helper <- function(name) {
  dir_path <- file_path(get_path_analysis_root(), "Helper Scripts")
  hook_path(name, dir_path = dir_path)
}

#' Source a Helper File
#'
#' Helper functions that facilitates interacting with the "Helper Scripts" subdirectory within an analysis
#' folder. You must be somewhere inside of an analysis folder, for these functions to work.
#' * `peek_helpers()` shows all available helper files in "Helper Scripts"
#' * `add_script_helper()` adds a script to "Helper Scripts"
#' * `source_helper()` sources a script from "Helper Scripts"
#' * `edit_helper()` opens a script from "Helper Scripts" for editing
#'
#' @param name For `add_script_helper()`, the name of the ".R" script to create in "Helper Scripts". For
#' `source_helper()` and `edit_helper()`, a pattern that uniquely matches exactly one script in
#' "Helper Scripts"
#' @name helper_scripts
NULL

#' @rdname helper_scripts
#' @export
peek_helpers <- function() {
  dir_path <- file_path(get_path_analysis_root(), "Helper Scripts")
  dir(dir_path)
}

#' @rdname helper_scripts
#' @export
add_script_helper <- function(name) {
  dir_path <- file_path(get_path_analysis_root(), "Helper Scripts")
  path <- file_path(dir_path, ensure_r(name))
  add_script(path)
}

#' @rdname helper_scripts
#' @export
source_helper <- function(name) {
  path <- get_path_helper(name)
  source(path)
}

#' @rdname helper_scripts
#' @export
edit_helper <- function(name) {
  path <- get_path_helper(name)
  file.edit(path)
}

