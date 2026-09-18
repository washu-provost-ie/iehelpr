#' Database Connection Helpers
#'
#' `db_connect()` connects to a database, given a `server` and `database`. After calling the function, you will need to authenticate
#' interactively by providing your WashU credentials. `db_connect()` and `sis_connect()` are convenient wrappers that connect to the
#' data warehouse and SIS archive, respectively. DO NOT assign the result - the connection will automatically be assigned a standard
#' name (`dw_connect()` creates a connection named `conn_dw` and `sis_connect()` creates a connection named `conn_sis`).
#' `dw_disconnect()` and `sis_disconnect()` disconnect from the warehouse and sis archive, respectively, and remove the names of the
#' connections from the environment.
#'
#' If the `uid` is not provided (or not valid), the function will still work. You will just need to go through an extra Microsoft Sign in
#' screen in which you provide your WashU email. The easiest way to always skip this step (and not manually enter your email as
#' the `uid` argument) is to store your email as the "ODBC_UID" parameter in the ".Renviron" file. To do that, open the ".Renviron"
#' file by calling `usethis::edit_r_environ()`. Then add this line to the file: ODBC_UID = "youremail@wustl.edu".
#' Save the ".Renvion" file and then restart R for the change to take effect.
#'
#' @param server,database strings giving the name of the server and database
#' @param uid your WashU email address. By default, the function retrieves this from ".Renviron". See `details`.
#' @param conn_name a string, "conn" by default. This will be the variable name for the new connection in the global environment.
#'
#' @returns Technically returns only `TRUE` if successful. It creates a database connection and assigns it to `conn_name` in the global
#' environment.
#' @export
db_connect <- function(server, database, uid = Sys.getenv("ODBC_UID"), conn_name = "conn") {
  if(conn_name %in% ls(name = rlang::global_env())) {
    cli::cli_inform("{.var {conn_name}} already exists")
    return(invisible(NULL))
  }

  drivers <- odbc::odbcListDrivers() |>
    dplyr::pull(name) |>
    unique() |>
    stringr::str_subset(pattern = "ODBC Driver.*for SQL Server")

  if(rlang::is_empty(drivers)) {
    cli::cli_abort("You must install an ODBC Driver for SQL Server")
  }

  driver <- drivers[1]

  if(uid %is% "") {
    cli::cli_inform(c("To skip the first authentication screen (the Microsoft Sign in), provide your WashU email as the uid",
                      "i" = "You may also save your email in your \".Renviron\"",
                      "i" = "call `usethis::edit_r_environ()` to open this file and add this line: ODBC_UID = [your Washu email]"))
  }

  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = driver,
    server = server,
    database = database,
    authentication = "ActiveDirectoryInteractive",
    Encrypt = "yes",
    TrustServerCertificate = "no",
    UID = uid
  )

  assign(conn_name, value = conn, pos = rlang::global_env())
  return(TRUE)
}

#' @rdname db_connect
#' @export
dw_connect <- function(uid = Sys.getenv("ODBC_UID")) {
  db_connect(server = "data-dw-prod-sql.database.windows.net", database = "data-dw-prod-dw", uid = uid, conn_name = "conn_dw")
}

#' @rdname db_connect
#' @export
sis_connect <- function(uid = Sys.getenv("ODBC_UID")) {
  db_connect(server = "data-archive-prod-sql.public.804549f24bfa.database.windows.net,3342", database = "Student_Info", uid = uid, conn_name = "conn_sis")
}

#' @rdname db_connect
#' @export
dw_disconnect <- function() {
  DBI::dbDisconnect(conn_dw)
  rm(conn_dw, pos = rlang::global_env())
  return(TRUE)
}

#' @rdname db_connect
#' @export
sis_disconnect <- function() {
  DBI::dbDisconnect(conn_sis)
  rm(conn_sis, pos = rlang::global_env())
  return(TRUE)
}


#' See Available Tables
#'
#' `dw_peek_census()` shows the names of all available packages in the "STUCENSUS" schema of the data warehouse.
#' `dw_peek_student()` shows the names of all available packages in the "STUDENT" schema of the warehouse (this is
#' the daily refresh of student data pulled from Workday).
#' `sis_peek_htv()` shows the names of all available SIS tables with an "htv_" prefix. NOTE: the appropriate
#' database connection (`conn_dw` or `conn_sis`) must be active
#'
#' @returns A character vector of table names
#' @name peek_database
NULL

#' @rdname peek_database
#' @export
dw_peek_census <- function() {
  DBI::dbListTables(conn_dw, schema_name = "STUCENSUS") |>
    stringr::str_subset(pattern = "^v")
}

#' @rdname peek_database
#' @export
dw_peek_student <- function() {
  DBI::dbListTables(conn_dw, schema_name = "STUDENT") |>
    stringr::str_subset("^cv")
}

#' @rdname peek_database
#' @export
sis_peek_htv <- function() {
  DBI::dbListTables(conn_sis) |>
    stringr::str_subset(pattern = "^htv_")
}

#' Extract Year or Term
#'
#' Given a vector of semesters (e.g., "Fall 2020", "Spring 2023", "FL25), `extract_year()` and
#' `extract_term()` extract the calendar year and term (e.g., "Fall" or "Spring"), respectively.
#' Semester inputs can be formatted flexibly as long as they are unambiguous.
#'
#' @param sem A vector containing semesters. This is failry flexbile but all inputs must have text
#' intepretatable as "Fall", "Spring", or "Summer" (e.g., "Fall", "F", "Spring", "sp") and must
#' include a valid 2-digit or 4-digit year.
#' @param is_2000 Should all dates be assumed to start with "20"? If `FALSE`, then all inputs
#' will need to include 4-digit years
#'
#' @returns For `extract_year()`, an integer vector of 4-digit years. For `extract_term()` a
#' character vector with only the values "Fall", "Spring" and "Summer"
#' @name sem_extract
NULL

#' @rdname sem_extract
#' @export
extract_year <- function(sem, is_2000 = TRUE) {
  # grab the numeric component
  year <- stringr::str_extract(sem, pattern = "\\d+")

  digits <- purrr::map_int(year, .f = stringr::str_length) |>
    unique() |>
    purrr::discard(.p = is.na)

  if(digits %not% 2 && digits %not% 4) {
    cli::cli_abort("{.arg x} must contain either all 2-digit years or all 4-digit years")
  }

  if(is_2000 && digits %is% 4 && !all(stringr::str_detect(year, pattern = "^20"), na.rm = TRUE)) {
    cli::cli_abort("if {.arg is_2000} is {.val {TRUE}}, all years must start with \"20\"")
  }

  if(digits %is% 2) {
    if(!is_2000) {
      cli::cli_abort("if {.arg is_2000} is {.val {FALSE}}, {.arg x} must contain all 4-digit years")
    }

    non_na <- !is.na(year)
    year[non_na] <- paste0("20", year[non_na])
  }

  # prevent warnings that would occur when year is NA
  out <- suppressWarnings(as.integer(year))
  return(out)
}

#' @rdname sem_extract
#' @export
extract_term <- function(sem, to_sis = FALSE) {
  fall <- ifelse(to_sis, "FL", "Fall")
  spring <- ifelse(to_sis, "SP", "Spring")
  summer <- ifelse(to_sis, "SU", "Summer")

  term <- stringr::str_extract(sem, "^\\w+") |>
    stringr::str_to_lower()

  if(any(!stringr::str_detect(term, "^f|^sp|^su"), na.rm = TRUE)) {
    cli::cli_abort('{.val {term}} not interpretable as ""Spring", "Summer", or "Fall"')
  }

  term[stringr::str_detect(term, pattern = "^f")] <- fall
  term[stringr::str_detect(term, pattern = "^sp")] <- spring
  term[stringr::str_detect(term, pattern = "^su")] <- summer

  return(term)
}

#' Convert Semester Values to Proper Format
#'
#' Given fairly flexible inputs (e.g., "fl23"), `sems_format()` converts to proper semester
#' format for the data warehouse ("Fall 2023") or, if`to_sis` is TRUE, SIS ("FL2023"). `sems_format_sis()`
#' is just a convenient wrapper where `to_sis` is TRUE.
#'
#' @param ... Either a character vector or individual string arguments
#' @param to_sis TRUE or FALSE - should the output be formatted to SIS format?
#'
#' @returns A character vector
#' @export
sems_format <- function(..., to_sis = FALSE) {
  sems <- dots_chr(...)
  years <- extract_year(sems)
  terms <- extract_term(sems, to_sis = to_sis)
  sep <- ifelse(to_sis, "", " ")
  out <- paste(terms, years, sep = sep)
  return(out)
}

#' @rdname sems_format
#' @export
sems_format_sis <- function(...) {
  sems_format(..., to_sis = TRUE)
}

#' Format SIS Semester Data into Data Warehouse Format
#'
#' Convert semesters from SIS format (e.g., "FL2020", "SP2020") to data warehouse
#' format (e.g., "Fall 2020", "Spring 2020")
#'
#' @param x a vector of semester data from an SIS table
#'
#' @returns A character vector with semesters in data warehouse format
#' @export
sems_harmonize <- function(x) {
  x |>
    str_replace_multi(
      "^FL" ~ "Fall ",
      "^SP" ~ "Spring ",
      "^SU" ~ "Summer "
    )
}


#' Format SIS Program Data in Data Warehouse Format
#'
#' Convert programs from SIS format (e.g., B.S. MAJOR IN CHEMISTRY) to data warehouse format
#' (e.g., Chemistry, B.S.). Note this does not directly link old SIS programs data warehouse programs,
#' it merely make the programs follow data warehouse format conventions (e.g., title case, degree at
#' the end preceded by a comma). If possible you should link SIS programs directly to their
#' data warehouse counterparts, e.g., by calling `fetch_programInfo_sis()` and using the `ProgName`
#' and `ProgName.warehouse` fields. The main purpose of `programs_harmonize` is to update the
#' formatting of SIS programs that you aren't able to match directly.
#'
#' @param x A character vector of SIS program names
#'
#' @returns A character vector
#' @export
programs_harmonize <- function(x) {
  format_prog <- function(x, delete_prefix, suffix) {
    delete_prefix <- paste0("^", delete_prefix, "\\s")
    is_update <- stringr::str_detect(x, pattern = delete_prefix)

    x[is_update] <- x[is_update] |>
      str_remove(pattern = delete_prefix) |>
      paste0(suffix)

    return(x)
  }

  x <- x |>
    str_to_title() |>
    format_prog(delete_prefix = "2nd Major In", suffix = " Second Major") |>
    format_prog(delete_prefix = "Second Major In", suffix = " Second Major") |>
    format_prog(delete_prefix = "A\\.b\\. Major In", suffix = ", A.B.") |>
    format_prog(delete_prefix = "B\\.f\\.a\\. Major In", suffix = ", B.F.A") |>
    format_prog(delete_prefix = "B\\.s\\. In", suffix = ", B.S.") |>
    format_prog(delete_prefix = "B\\.s\\.b\\.a\\. Major In", suffix = " Second Major") |>
    format_prog(delete_prefix = "B\\.s\\. Major In", suffix = ", B.S.") |>
    stringr::str_replace(pattern = "Bjc", replacement = "BJC") |>
    replace_values("B.s. Undeclared Major" ~ "Undeclared Major, B.S.")

  return(x)
}

# Note, this is just a helper, I don't think I need to document/export
next_sem <- function(sem, to_sis = FALSE, include_summer = FALSE) {
  fall <- ifelse(to_sis, "FL", "Fall ")
  spring <- ifelse(to_sis, "SP", "Spring ")
  summer <- ifelse(to_sis, "SU", "Summer ")

  sem <- sems_format(sem, to_sis = to_sis)

  if(!include_summer && stringr::str_detect(sem, pattern = glue::glue("^{summer}"))) {
    cli::cli_abort("You can't include a summer semester unless {.arg include_summer} is {.val {TRUE}}")
  }

  c(string, term, year) %<-% stringr::str_match(sem, pattern = "(.*)(\\d\\d\\d\\d)$")
  year <- as.integer(year)

  next_year <- ifelse(term %is% fall, year + 1, year)

  if(!include_summer) next_term <- dplyr::replace_values(term, fall ~ spring, spring ~ fall)
  else next_term <- dplyr::replace_values(term, fall ~ spring, spring ~ summer, summer ~ fall)

  return(paste0(next_term, next_year))
}

#' Expand Semesters that Include ":" Inputs
#'
#' This function allows the use of `:` to refer to consecutive semesters. (e.g. FL23:SP25 to refer
#' to "Fall 2023", "Spring 2024", "Fall 2025", and "Spring 2025"). `sems_expand_sis()` is just a
#' convenient wrapper where `to_sis` is `TRUE`
#'
#' @param ... semesters entered as strings, symbols, or character vectors. `:` expressions will be expanded
#' to include all consecutive semesters (e.g., `fl20:sp22` would include "Fall 2020", "Spring 2021", "Fall 2021", and "Spring 2022")
#' @param to_sis TRUE or FALSE - should the output be in SIS format?
#' @param include_summer should summer semesters be included when expanding consecutive semesters?
#' `FALSE` by default since summer semesters are not of interest in most analyses
#'
#' @returns A character vector of semesters in the desired format
#' @export
sems_expand <- function(..., to_sis = FALSE, include_summer = FALSE) {
  dots <- rlang::enquos(...)

  sem_list <- dots |>
    # evaluate evaluable things and convert to a character vector. Deparse nonevaluable things
    purrr::map(.f = \(x) {
      if(is_evaluable(x)) x <- as.character(rlang::eval_tidy(x))
      else x <- deparse1(rlang::get_expr(x))

      return(x)
    }) |>
    # collect into a character vector - note some elements may be ":" strings
    purrr::list_c(ptype = character()) |>
    # expand ":" and put everything in proper format
    purrr::map(.f = \(x) {
      if(!stringr::str_detect(x, pattern = ":")) {
        x <- sems_format(x, to_sis = to_sis)
        if(!include_summer && stringr::str_detect(x, pattern = "SU|Summer")) {
          cli::cli_abort("You can't include summer semesters if {.arg include_summer} is {.val {FALSE}}")
        }

        return(x)
      }

      x <- x |>
        stringr::str_split_1(pattern = ":") |>
        sems_format(to_sis = to_sis)

      if(!include_summer && any(stringr::str_detect(x, pattern = "SU|Summer"))) {
        cli::cli_abort("You can't include summer semesters if {.arg include_summer} is {.val {FALSE}}")
      }

      # ":" elements will now be length 2. Pull them out into start and end variables
      c(start, end) %<-% x



      # initialize "curr" as the starting semester
      curr <- start

      # a character vector to build up to include all the sequential semesters. Initialize with `curr`
      sems <- curr
      while(TRUE) {
        # find the next semester and add to `sems`
        curr <- next_sem(curr, to_sis = to_sis, include_summer = include_summer)


        sems <- c(sems, curr)

        # once the "next semester" we've just added matches `end`, then we're done!
        if(curr %is% end) {
          break
        }
      }

      return(sems)
    }) |>
    # collect into a character vector - now every element is a single semester
    purrr::list_c(ptype = character())

  if(!include_summer && any(stringr::str_detect(sem_list, pattern = "SU|Summer"))) {
    cli::cli_abort("You can't include summer semesters if {.arg include_summer} is {.val {FALSE}}")
  }

  return(sem_list)
}

#' @rdname sems_expand
#' @export
sems_expand_sis <- function(..., include_summer = FALSE) {
  sems_expand(..., to_sis = TRUE, include_summer = include_summer)
}


#' Filter a Data Warehouse or SIS Table by Semester
#'
#' Filter an SIS or data warehouse table by semester. `...` arguments are processed with [sems_expand()],
#' allowing flexible and efficient generation of many semsters. `filter_sems_sis()` is just a wrapper for
#' `filter_sems()` with `to_sis` set to TRUE.
#'
#' @param x Data Warehouse or SIS Data as a remote table or dataframe
#' @param ... semesters entered as strings, symbols, or character vectors. `:` expressions will be expanded
#' to include all consecutive semesters (e.g., `fl20:sp22` would include "Fall 2020", "Spring 2021", "Fall 2021", and "Spring 2022")
#' @param to_sis TRUE or FALSE - should the output be in SIS format?
#' @param include_summer should summer semesters be included when expanding consecutive semesters?
#' `FALSE` by default since summer semesters are not of interest in most analyses
#' @param sem_col the name of the semester column as a string or symbol. Defaults guesses are made based
#' on the `to_sis` argument
#'
#' @returns A tibble or remote table (same as `x`)
#' @export
filter_sems <- function(x, ..., to_sis = FALSE, include_summer = FALSE, sem_col = waiver()) {
  sem_col <- rlang::enquo(sem_col)

  if(quo_is_waiver(sem_col)) sem_col <- ifelse(to_sis, rlang::expr(DispSem), rlang::expr(StandardAcademicPeriod))
  else sem_col <- rlang::sym(rlang::get_expr(sem_col))

  sems <- sems_expand(..., to_sis = to_sis, include_summer = include_summer)

  dplyr::filter(x, !!sem_col %in% sems)
}

#' @rdname filter_sems
#' @export
filter_sems_sis <- function(x, ..., include_summer = FALSE, sem_col = DispSem) {
  filter_sems(x, ..., to_sis = TRUE, include_summer = include_summer, sem_col = {{sem_col}})
}


#' Collect an SIS Table and Make More Consistent with Data Warehouse
#'
#' A wrapper for [dplyr::collect()] that makes a couple of tweaks to make SIS source
#' data more consistent with warehouse data. Namely, it renames `ID` to `StudentID` and
#' ensures that it is a character vector. Also if `DispSem` exists it is renamed to
#' `StandardAcademicPeriod` and formatted from SIS style (e.g., "FL2020") to Data
#' Warehouse style (e.g., "Fall 2020")
#'
#' @param x A remote SIS table
#' @param ... arguments passed to [dplyr::collect()]
#'
#' @returns A tibble
#' @export
collect_sis <- function(x, ...) {
  x <- dplyr::collect(x, ...) |>
    dplyr::mutate(ID = as.character(ID)) |>
    dplyr::rename(StudentID = ID)

  if("DispSem" %in% names(x)) {
    x <- x |>
      dplyr::mutate(DispSem = sems_harmonize(DispSem)) |>
      dplyr::rename(StandardAcademicPeriod = DispSem)
  }

  return(x)
}


#' Retrieve a Census Package from the Data Warehouse
#'
#' `dw_tbl_census()` creates a `tbl` object linked to a given census package. You can interact with this object via
#' 'dplyr' verbs as needed and then call [dplyr::collect()] to pull the result into a data frame. Commonly, you will use
#' `filter()` and `select()` to narrow the data before using `collect()`.
#' By default, this will filter the data to the Fall 2025 10th week census, but this can be adjusted to include any census
#' or censuses of interest. These are convenient wrappers to link to specific packages:
#' * `dw_tbl_census_AcademicRecord()` for "vAcademicRecord"
#' * `dw_tbl_census_AcademicPeriodRecord()` for "vAcademicPeriodRecord"
#' * `dw_tbl_census_ProgramOfStudyRecord()` for "vProgramOfStudyRecord"
#' * `dw_tbl_census_RegistrationRecord()` for "vRegistrationRecord"
#' * `dw_tbl_census_SectionRoleAssignment()` for "vSectionRoleAssignment"
#'
#' @param pkg pattern matching the name of exactly table in the "STUCENSUS" schema
#' @param sems character vector giving the snapshot semesters to include. This is fairly flexible, but each element
#' must include text that's interpretable as either "Fall" or "Spring" and a year (either 2- or 4-digits). You can
#' use ":" format to include consecutive semesters
#' @param weeks character or numeric vector giving the weeks to include. Can include only 0, 4, or 10
#' (or string variants such as "0", "4", "10" or "00", "04", "10").
#' @param conn a database connection to the the data warehouse (called `conn_dw` by default)
#'
#' @returns A `tbl` object.
#' @export
dw_tbl_census <- function(pkg, sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  census_packages <- dw_peek_census()

  pkg <- hook(pkg, table = census_packages)
  tbl_name <- DBI::Id(schema = "STUCENSUS", table = pkg)

  cli::cli_inform("connecting to table {.val STUCENSUS}.{.val {pkg}}")
  cat("\n")

  tbl_census <- dplyr::tbl(conn, tbl_name)

  weeks <- purrr::map_chr(weeks, .f = purrr::partial(stringr::str_pad, width = 2, pad = "0"))
  tbl_census <- dplyr::filter(tbl_census, SnapshotWeek %in% .env$weeks)
  cli::cli_inform(c("census output limited to week{?s} {.val {weeks}}",
                    "update the {.arg weeks} argument to adjust this"))

  cat("\n")

  sems <- rlang::enquo(sems)

  if(is_evaluable(sems)) {
    res <- rlang::eval_tidy(sems)
    if(res %is% ".all" || rlang::is_empty(res)) {
      cli::cli_inform(c("census output includes all available semesters",
                        "update the {.arg sems} argument to adjust this"))

      return(tbl_census)
    }
  }

  sems <- sems_expand(!!sems)
  tbl_census <- dplyr::filter(tbl_census, paste(SnapshotSemester, SnapshotYear) %in% sems)

  cli::cli_inform(c("census output limited to semester{?s} {.val {sems}}",
                    "update the {.arg sems} argument to adjust this"))

  return(tbl_census)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_census_AcademicRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "AcademicRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_census_AcademicPeriodRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "AcademicPeriodRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_census_ProgramOfStudyRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "ProgramOfStudyRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_census_RegistrationRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "RegistrationRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_census_SectionRoleAssignment <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "SectionRoleAssignment", sems = sems, weeks = weeks, conn = conn)
}


#' Retrieve a Student Data Package from the Warehouse
#'
#' Creates a `tbl` object linked to a given student data package (Workday daily refresh tables). You can
#' interact with this object via `dplyr` verbs as needed and then call [dplyr::collect()] to pull the result
#' into a data frame. Commonly, you will use `filter()` and `select()` to narrow the data before using `
#' collect()`.
#' These are convenient wrappers to link to specific packages:
#' * `dw_tbl_student_AcademicRecord()` for "cvAcademicRecord"
#' * `dw_tbl_student_AcademicPeriodRecord()` for "cvAcademicPeriodRecord"
#' * `dw_tbl_student_CourseSectionDefinition()` for "cvCourseSectionDefinitionStudyRecord"
#' * `dw_tbl_student_ProgramOfStudyDefinition()` for "cvProgramOfStudyDefinition"
#' * `dw_tbl_student_RegistrationRecord()` for "cvRegistrationRecord"
#' * `dw_tbl_student_WaitlistUtilization()` for "cvWaitlistUtilization"
#'
#' @param pkg pattern matching the name of exactly table in the "STUDENT" schema
#' @param conn a database connection to the the data warehouse (called `conn_dw` by default)
#'
#' @returns A `tbl` object
#' @export
dw_tbl_student <- function(pkg, conn = conn_dw) {
  packages <- dw_peek_student()

  pkg <- hook(pkg, table = packages)
  tbl_name <- DBI::Id(schema = "STUDENT", table = pkg)

  cli::cli_inform("connecting to table {.val STUDENT}.{.val {pkg}}")
  cat("\n")

  dplyr::tbl(conn, tbl_name)
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_AcademicRecord <- function(conn = conn_dw) {
  dw_tbl_student("cvAcademicRecord")
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_AcademicPeriodRecord <- function(conn = conn_dw) {
  dw_tbl_student("cvAcademicPeriodRecord")
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_CourseSectionDefinition <- function(conn = conn_dw) {
  dw_tbl_student("cvCourseSectionDefinition")
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_ProgramOfStudyDefinition <- function(conn = conn_dw) {
  dw_tbl_student("cvProgramOfStudyDefinition")
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_RegistrationRecord <- function(conn = conn_dw) {
  dw_tbl_student("cvRegistrationRecord")
}

#' @rdname dw_tbl_student
#' @export
dw_tbl_student_WaitlistUtilization <- function(conn = conn_dw) {
  dw_tbl_student("cvWaitlistUtilization")
}



#' Retrieve an "htv_" table from the SIS Archive
#'
#' Creates a `tbl` object linked to a given "htv_" table. You can interact with this object via dplyr verbs as
#' needed and then call [dplyr::collect()] to pull the result into a data frame. Commonly, you will use
#' `filter()` and `select()` to narrow the data before using `collect()`.
#' These are convenient wrappers to link to commonly used tables:
#' * `sis_tbl_degree()` for "htv_degree_info"
#' * `sis_tbl_aa()` for "htv_academic_action"
#' * `sis_tbl_milestones()` for "htv_milestones"
#' * `sis_tbl_progHist()` for "student_prog_hist"
#' * `sis_tbl_divHist()` for "stdt_div_hist"
#' * `sis_tbl_demos()` for "stdt_demographics"
#' * `sis_tbl_scores()` for "htv_scores"
#' * `sis_tbl_courseRecord()` for "htv_stdt_course_rec"
#'
#' @param table pattern matching the name of exactly one "htv_" table in the SIS archive
#' @param conn a database connection to the the SIS archive.
#'
#' @returns A `tbl()` object.
#' @export
sis_tbl_htv <- function(table, conn = conn_sis) {
  tables <- sis_peek_htv()
  tbl_name <- hook(table, table = tables)

  cli::cli_inform("connecting to table {.val {tbl_name}}")
  cat("\n")

  dplyr::tbl(conn, tbl_name)
}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_degree <- function(conn = conn_sis) {sis_tbl_htv("degree_info", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_aa <- function(conn = conn_sis) {sis_tbl_htv("htv_academic_action", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_milestones <- function(conn = conn_sis) {sis_tbl_htv("milestones", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_progHist <- function(conn = conn_sis) {sis_tbl_htv("student_prog_hist", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_divHist <- function(conn = conn_sis) {sis_tbl_htv("stdt_div_hist", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_demos <- function(conn = conn_sis) {sis_tbl_htv("stdt_demographics", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_scores <- function(conn = conn_sis) {sis_tbl_htv("htv_scores", conn = conn_sis)}

#' @rdname sis_tbl_htv
#' @export
sis_tbl_courseRecord <- function(conn = conn_sis) {sis_tbl_htv("htv_stdt_course_rec", conn = conn_sis)}


#' Database Helpers
#'
#' Helpers to get information about the current state of a `tbl` without using `collect()` to
#' pull it into a tibble/data frame.
#' * `db_names()` gives the column names (alias for `colnames()`)
#' * `db_nrow()` gives the number of rows
#' * `db_col_vals()` shows all the unique values in a given column
#' * `db_count()` works just like [dplyr::count()] for remote tables, but returns as a data frame
#' (rather than `tbl`) to facilitate printing of the full counts table
#'
#' @param tbl A `tbl` object linked to the table of a database
#' @param col Column name as a string or symbol
#' @inheritParams dplyr::count
#'
#' @name database_helpers
NULL

#' @rdname database_helpers
#' @export
db_names <- function(tbl) {
  colnames(tbl)
}

#' @rdname database_helpers
#' @export
db_nrow <- function(tbl) {
  tbl |>
    dplyr::summarize(n = n()) |>
    dplyr::pull(n) |>
    as.integer()
}

#' @rdname database_helpers
#' @export
db_col_vals <- function(tbl, col) {
  tbl |>
    dplyr::pull({{col}}) |>
    unique() |>
    sort()
}

#' @rdname database_helpers
#' @export
db_count <- function(tbl, ..., wt = NULL, sort = FALSE, name = NULL) {
  # for some reason, it did not work to pass wt = NULL to dplyr::count when inside of a function
  if(is.null(wt)) wt <- 1

  dplyr::count(tbl, ..., wt = wt, sort = sort, name = name) |>
    dplyr::collect() |>
    as.data.frame()
}

#' Read the "RDS file" provided annually by Ryan Croft
#'
#' @param year which year to read. By default will read the most recent year available
#' @param is_rds TRUE or FALSE - should you read the RDS version of the file? This is
#' recommended if possible because it avoids any conversion errors
#'
#' @returns A data frame
#' @export
fetch_grs <- function(year = waiver(), is_rds = TRUE) {
  box_dir <- Sys.getenv("BOX_DIR")
  grs_dir <- file_path(box_dir, "00 IE", "Student Outcomes Team_SHARED", "Central Data Sources", "GRS Report Data",
                       "OUR_provided_archive", "03 Cleaned Data")

  sem_dirs <- dir(grs_dir)

  if(is_waiver(year)) {
    year <- sem_dirs |>
      readr::parse_number() |>
      sort() |>
      dplyr::last() |>
      as.character()
  }

  sem_dir <- str_subset1(sem_dirs, pattern = year)
  data_dir <- file_path(grs_dir, sem_dir)
  if(is_rds) data_dir <- file_path(data_dir, "rds Versions")

  read_fn <- ifelse(is_rds, readRDS, readr::read_csv)

  path <- hook_path("\\d\\d\\d\\d\\.", dir_path = data_dir)
  read_fn(path)
}

#' Retrieve SIS and Workday Program of Study Inventories
#'
#' These functions retrieve full program inventories for SIS and the Data Warehouse. Because the SIS archive is
#' frozen, the program inventory has been saved to file, and `fetch_programInfo_sis()` retrieves and reads
#' this static file from "OO IR Office/Data Sources, Resources/SOURCE-SIS Archive". `fetch_programInfo_workday()`
#' reads live data from the data warehouse ("STUDENT"."cvProgramOfStudyDefinition"), since the workday program
#' info can be updated at any time.
#'
#' @returns A tibble
#' @name fetch_programInfo
NULL

#' @rdname fetch_programInfo
#' @export
fetch_programInfo_sis <- function() {
  box_dir <- Sys.getenv("BOX_DIR")
  dir_path <- file_path(box_dir, "00 IR Office", "Data Sources, Resources", "SOURCE-SIS Archive")
  prog_info <- read_match("ProgramOfStudyInventory_SIS", dir_path = dir_path)
  return(prog_info)
}

#' @rdname fetch_programInfo
#' @export
fetch_programInfo_workday <- function() {
  ################# Read and Reorder Columns ###################
  prog_info <- dw_tbl_student_ProgramOfStudyDefinition() |>
    dplyr::select(Program_of_Study_Code, Program_of_Study, Program_of_Study_ID, Program_of_Study_Name,
                  Program_of_Study_Academic_Level,
                  Program_of_Study_Owning_School, Program_of_Study_Owning_AU, Program_of_Study_Coordinating_AU,
                  Taxonomy_Code, Taxonomy, CIP_Code, CIP_Title, everything()) |>
    dplyr::collect()

  # SIS columns that we want to link to the Data Warehouse Program Info
  sis_info <- fetch_programInfo_sis() |>
    dplyr::select(ProgCode, ProgName)

  ############## Take First "Guess" at Values in the Linking Field
  prog_info <- prog_info |>
    dplyr::mutate(
      ProgCode.sis = stringr::str_remove(Program_of_Study_ID, pattern = "^POS_"),
      .after = Program_of_Study
    )

  ####### Check Values in the Linking Field ###############################
  # Set the "guessed" ProgCode to NA if it's not actually in the the SIS program info
  is_in_sis <- prog_info$ProgCode.sis %in% sis_info$ProgCode
  prog_info$ProgCode.sis[!is_in_sis] <- NA_character_


  ########## Use the Verified Linking Values to Pull in ProgName Field from SIS ##############
  # add ".sis" suffix to make it clear that these are fields originally coming from SIS
  sis_info <- dplyr::select(sis_info, ProgCode.sis = ProgCode, ProgName.sis = ProgName)
  prog_info <- left_join2(prog_info, sis_info, by = "ProgCode.sis", .after = ProgCode.sis)

  return(prog_info)
}


#' Fetch Useful Datasets from Data Warehouse or SIS
#'
#' Whereas `sis_dw_` and `sis_tbl_` functions connect to tables as-is and return a `tbl` connection object, `fetch_` functions
#' are designed to return more "analysis ready" tibbles. They do things such as convert SIS data to data warehouse format
#' (e.g., making column names and date formats consistent), join in supplementary data, order columns based on importance /
#' frequencey of use, and perform other data manipulation to output data that is ready-made for a specific purpose.
#'
#' * `fetch_progHist_sis()` pulls in a transformed version of "htv_student_prog_hist"
#' * `fetch_degrees_sis()` pulls in a transformed version of "htv_degree_info"
#'
#' @param sems semesters to include. Can include consecutive semesters with ":" syntax
#' @param add_program_info TRUE or FALSE - should supplementary program info columns be added?
#'
#' @returns A tibble
#' @name fetch_sis
NULL

#' @rdname fetch_sis
#' @export
fetch_progHist_sis <- function(sems = f13:sp25, add_program_info = TRUE) {
  hist <- sis_tbl_progHist() |>
    filter_sems_sis({{sems}}) |>
    collect_sis()

  if(add_program_info) {
    prog_info <- fetch_programInfo_sis()
    hist <- dplyr::left_join(hist, prog_info, by = "ProgCode")
  }

  return(hist)
}

#' @rdname fetch_sis
#' @export
fetch_progHist_dw <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census_ProgramOfStudyRecord(sems = {{sems}}, weeks = weeks, conn = conn) |>
    select(StudentID, StandardAcademicPeriod, SnapshotCode, ProgramOfStudyCode, ProgramOfStudy,
           ReportingRecordFlag, PrimaryProgramOfStudyFlag, AcademicRecordAcademicLevel, EducationalTaxonomyCodeID,
           EducationalTaxonomyCode, ProgramOfStudyOwningSchool, ProgramOfStudyCoordinatingAcademicUnit,
           ProgramOfStudyType, CIPCode, everything()) |>
    collect()
}

#' @rdname fetch_sis
#' @export
fetch_degrees_sis <- function(sems = f13:su25, add_program_info = TRUE) {
  degrees <- sis_tbl_degree() |>
    filter_sems_sis({{sems}}, include_summer = TRUE) |>
    collect_sis() |>
    rename(ProgCode = ProgramCd, ProgName = ProgramName)

  if(add_program_info) {
    prog_info <- fetch_programInfo_sis()
    degrees <- degrees |>
      # I will pick up ProgName from the join
      select(-ProgName) |>
      left_join(prog_info, by = "ProgCode") |>
      select(StudentID, StandardAcademicPeriod, SortSem, ProgCode, ProgName, Program_of_Study_Code.dw:CIP2000,
             everything())|>
      arrange(StudentID, StandardAcademicPeriod, ProgCode)
  }

  return(degrees)
}



#' Compute Useful Variables from Semesters
#'
#' Given a vector of semesters, these functions compute useful variables.
#'
#' `compute_AY` returns the academic year in typical text format (e.g., "AY 2024-25"). `compute_AY_startYear()`
#' computes the starting year for the academic year (e.g., 2020 for both "Fall 2020" and "Spring 2021").
#' Given both a vector a semesters and a second vector representing students' entry semesters,
#' `compute_student_year()` and `compute_student_semester()` calculate the students' year number (e.g.,
#' a student in their first year gets a `1`) and semester number (e.g., a student in their third semesters
#' gets a `3`), respectively.
#'
#' The primary reason to use `compute_AY_startYear()` is when you want to use the output in further calculations.
#' Otherwise, `compute_AY()` is usually preferable, especially if the output could be used in labels
#' (e.g., in a plot or table).
#'
#' For `compute_student_semester()`, summer semesters are counted as semester number 2.5 of the academic year.
#' That way, every year counts as 2 semesters (e.g., A student's second Fall is their third semester), but there
#' is still a way to represent summer semesters.
#'
#' @param sem A vector of semesters (e.g., StandardAcademicPeriod)
#' @param prefix A string to add as a prefix to the output. By default, this is "AY " (the space is intentional),
#' and the output will be formatted like "AY 2020-21". Set prefix to "" or `NULL` to return just "2020-21", set
#' it to "AY" (no space) to return "AY2020-21", or use the `prefix` argument to set any other custom prefix
#' @param entry_sem A vector representing entry semesters (e.g, cohort)
#'
#' @returns A numeric vector
#' @name semester_computations
NULL

#' @rdname semester_computations
#' @export
compute_AY_startYear <- function(sem) {
  year <- extract_year(sem)
  term <- extract_term(sem)
  is_not_fall <- !is.na(term) & term != "Fall"
  year[is_not_fall] <- year[is_not_fall] - 1
  return(year)
}

#' @rdname semester_computations
#' @export
compute_AY <- function(sem, prefix = "AY ") {
  start <- compute_AY_startYear(sem)

  is_na <- is.na(start)

  end <- stringr::str_remove(start + 1, pattern = "^\\d\\d")
  AY <- paste(start, end, sep = "-")
  if(!is.null(prefix)) AY <- paste0(prefix, AY)

  AY[is_na] <- NA
  return(AY)
}

#' @rdname semester_computations
#' @export
compute_student_year <- function(sem, entry_sem) {
  sem_AY <- compute_AY_startYear(sem)
  entry_AY <- compute_AY_startYear(entry_sem)
  student_year <- sem_AY - entry_AY + 1L
  return(student_year)
}

#' @rdname semester_computations
#' @export
compute_student_sem <- function(sem, entry_sem) {
  term_vals <- c(Fall = 1, Spring = 2, Summer = 2.5)

  AY_sem <- compute_AY_startYear(sem)
  AY_entry <- compute_AY_startYear(entry_sem)
  AY_diff <- AY_sem - AY_entry

  term_sem <- extract_term(sem)
  term_entry <- extract_term(entry_sem)
  term_entry[term_entry == "Summer"] <- "Fall"

  term_vals_sem <- unname(term_vals[term_sem])
  term_vals_entry <- unname(term_vals[term_entry])
  term_adjust <- term_vals_sem - term_vals_entry

  out <- as.double(1 + 2 * AY_diff + term_adjust)
  return(out)
}

