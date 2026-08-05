#' Database Connection Helpers
#'
#' `connect_db()` connects to a database, given a `server` and `database`. After calling the function, you will need to authenticate
#' interactively by providing your WashU credentials. `connect_db()` and `connect_sis()` are convenient wrappers that connect to the
#' data warehouse and SIS archive, respectively. DO NOT assign the result - the connection will automatically be assigned a standard
#' name (`connect_dw()` creates a connection named `conn_dw` and `connect_sis()` creates a connection named `conn_sis`).
#' `disconnect_dw()` and `disconned_sis()` disconnect from the warehouse and sis archive, respectively, and remove the names of the
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
connect_db <- function(server, database, uid = Sys.getenv("ODBC_UID"), conn_name = "conn") {
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

#' @rdname connect_db
#' @export
connect_dw <- function(uid = Sys.getenv("ODBC_UID")) {
  connect_db(server = "data-dw-prod-sql.database.windows.net", database = "data-dw-prod-dw", uid = uid, conn_name = "conn_dw")
}

#' @rdname connect_db
#' @export
connect_sis <- function(uid = Sys.getenv("ODBC_UID")) {
  connect_db(server = "data-archive-prod-sql.public.804549f24bfa.database.windows.net,3342", database = "Student_Info", uid = uid, conn_name = "conn_sis")
}

#' @rdname connect_db
#' @export
disconnect_dw <- function() {
  DBI::dbDisconnect(conn_dw)
  rm(conn_dw, pos = rlang::global_env())
  return(TRUE)
}

#' @rdname connect_db
#' @export
disconnect_sis <- function() {
  DBI::dbDisconnect(conn_sis)
  rm(conn_sis, pos = rlang::global_env())
  return(TRUE)
}


#' See Available Tables
#'
#' `dw_peek_census()` shows the names of all available packages in the "STUCENSUS" schema of the data warehouse.
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
sis_peek_htv <- function() {
  DBI::dbListTables(conn_sis) |>
    stringr::str_subset(pattern = "^htv_")
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
  fall <- ifelse(to_sis, "FL", "Fall ")
  spring <- ifelse(to_sis, "SP", "Spring ")
  summer <- ifelse(to_sis, "SU", "Summer ")

  sems <- dots_chr(...) |>
    purrr::map_chr(.f = \(x) {
      sem <- x |>
        stringr::str_extract(pattern = "[[:alpha:]]+") |>
        tolower()

      if(stringr::str_detect(sem, pattern = "^f")) sem <- fall
      else if(stringr::str_detect(sem, pattern = "^sp")) sem <- spring
      else if(stringr::str_detect(sem, pattern = "^su")) sem <- summer
      else {
        cli::cli_abort(c("The semester {.val {x}} is not interpretable as a valid semester",
                         "i" = "adjust the {.arg sems} argument"))
      }

      year <- extract_year(x)

      return(paste0(sem, year))
    })

  return(sems)
}

#' @rdname sems_format
#' @export
sems_format_sis <- function(...) {
  sems_format(..., to_sis = TRUE)
}


#' Convert Semester Data from SIS Format to Data Warehouse Format
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
    stringr::str_replace("^FL", replacement = "Fall ") |>
    stringr::str_replace("^SP", replacement = "Spring ") |>
    stringr::str_replace("^SU", replacement = "Summer ")
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
#' Creates a `tbl` object linked to a given census package. You can interact with this object via dplyr verbs as
#' needed and then call [dplyr::collect()] to pull the result into a data frame. Commonly, you will use
#' `filter()` and `select()` to narrow the data before using `collect()`.
#' By default, this will filter the data to the Fall 2025 10th week census, but this can be adjusted to include any census
#' or censuses of interest. These are convenient wrappers to link to specific packages:
#' * `dw_tbl_AcademicRecord()` for "vAcademicRecord"
#' * `dw_tbl_AcademicPeriodRecord()` for "vAcademicPeriodRecord"
#' * `dw_tbl_ProgramOfStudyRecord()` for "vProgramOfStudyRecord"
#' * `dw_tbl_RegistrationRecord()` for "vRegistrationRecord"
#' * `dw_tbl_SectionRoleAssignment()` for "vSectionRoleAssignment"
#'
#' @param pkg pattern matching the name of exactly table in the "STUCENSUS" schema
#' @param sems character vector giving the semesters to include. This is fairly flexible, but each element
#' must include text that's interpretable as either "Fall" or "Spring" and a year (either 2- or 4-digits).
#' @param weeks character or numeric vector giving the weeks to include. Can include only 0, 4, or 10
#' (or string variants such as "0", "4", "10" or "00", "04", "10").
#' @param conn a database connection to the the SIS archive.
#'
#' @returns A `tbl()` object.
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
dw_tbl_AcademicRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "AcademicRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_AcademicPeriodRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "AcademicPeriodRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_ProgramOfStudyRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "ProgramOfStudyRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_RegistrationRecord <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "RegistrationRecord", sems = sems, weeks = weeks, conn = conn)
}

#' @rdname dw_tbl_census
#' @export
dw_tbl_SectionRoleAssignment <- function(sems = "Fall 2025", weeks = 10, conn = conn_dw) {
  dw_tbl_census(pkg = "SectionRoleAssignment", sems = sems, weeks = weeks, conn = conn)
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
#' These functions retrieve program inventory files from "OO IR Office/Data Sources, Resources/SOURCE-Workday Student/Program Inventory".
#' These files were pulled from the source data (SIS Archive "htv_programs" table and Workday report SRPT0027) on 8/4/2026 and and slightly updated
#' (remove, rearrange columns and added columns to facilitate linking programs across SIS and Workday). One column in the SIS data (ProgramType.revised)
#' was cleaned to better match Workday categories, but the original column is also retained.
#'
#' @returns A tibble
#' @name fetch_programInfo
NULL

#' @rdname fetch_programInfo
#' @export
fetch_programInfo_sis <- function() {
  box_dir <- Sys.getenv("BOX_DIR")
  dir_path <- file_path(box_dir, "00 IR Office", "Data Sources, Resources", "SOURCE-Workday Student", "Program Inventory")
  prog_info <- read_match("ProgramOfStudyInventory_SIS", dir_path = dir_path)
  return(prog_info)
}

#' @rdname fetch_programInfo
#' @export
fetch_programInfo_workday <- function() {
  box_dir <- Sys.getenv("BOX_DIR")
  dir_path <- file_path(box_dir, "00 IR Office", "Data Sources, Resources", "SOURCE-Workday Student", "Program Inventory")
  prog_info <- read_match("ProgramOfStudyInventory_Workday", dir_path = dir_path)
  return(prog_info)
}

#' Fetch "htv_progHist" and Supporting Info
#'
#' This function automatically collects the "htv_student_prog_hist" table from the SIS archive, harmonizes it with the
#' data warehouse (via [collect_sis()]), and optionally adds in additional program info (see [fetch_programInfo_workday()]).
#'
#' @param sems semesters to include. Can include consecutive semesters with ":" syntax
#' @param add_program_info TRUE or FALSE - should supplementary program info columns be added?
#'
#' @returns A tibble
#' @export
fetch_progHist_sis <- function(sems = fl13:sp24, add_program_info = TRUE) {
  hist <- sis_tbl_progHist() |>
    filter_sems_sis({{sems}}) |>
    collect_sis()

  if(add_program_info) {
    prog_info <- fetch_programInfo_sis()
    hist <- dplyr::left_join(hist, prog_info, by = "ProgCode")
  }

  return(hist)
}


