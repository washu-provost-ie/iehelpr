dots_flat <- function(..., .ptype = NULL) {
  dots <- rlang::list2(...)

  if(is_length1(dots)) dots <- dots[[1]]

  if(!is_flat(dots)) {
    cli::cli_abort(c("`...` could not be flattened",
                   "i" = "`...` must contain only scalar individual arguments or a single flat vector/list"))
  }

  if(!is.null(.ptype)) dots <- as_vec(dots, .ptype = .ptype)
  else dots <- as.list(dots)

  return(dots)
}



