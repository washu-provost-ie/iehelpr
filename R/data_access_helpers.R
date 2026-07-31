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
    invisible(NULL)
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

  if(rlang::is_empty(sems) || sems %is% ".all") {
    cli::cli_inform(c("census output includes all available semesters",
                      "update the {.arg sems} argument to adjust this"))

    return(tbl_census)
  }

  sems <- purrr::map_chr(sems, .f = \(x) {
    sem <- x |>
      stringr::str_extract(pattern = "[[:alpha:]]+") |>
      tolower()

    if(stringr::str_detect(sem, pattern = "^f")) sem <- "Fall"
    else if(stringr::str_detect(sem, pattern = "^sp")) sem <- "Spring"
    else {
      cli::cli_abort(c("The semester {.val {x}} is not interpretable as \"Fall\" or \"Spring\"",
                       "i" = "adjust the {.arg sems} argument"))
    }

    year <- stringr::str_extract(x, pattern = "[[:digit:]]+")
    if(stringr::str_length(year) %is% 2) year <- paste0("20", year)

    return(paste(sem, year))
  })

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















