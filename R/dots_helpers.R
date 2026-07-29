#' Process `...` Arguments as a Flat List or Vector
#'
#' This function accepts two different forms of `...` and ensures that the input is processed as a flat list or vector.
#' `...` can be either a single vector/list or a series of scalar elements. If NOT a single vector/list, then all
#' elements must be scalar, and the individual elements will be compiled into a vector or flat list.
#'
#' Formulas are considered scalar in the context of [dots_flat()]. See [is_scalar()].
#'
#' @param ... A vector, a flat list, or a series of scalar arguments
#' @param .ptype Prototype giving the desired type for the output vector. If `NULL` (the default), the
#' output will be a flat list
#'
#' @returns A vector or flat list
#' @export
dots_flat <- function(..., .ptype = NULL) {
  dots <- rlang::list2(...)

  if(is_single(dots)) dots <- dots[[1]]

  if(!is_flat(dots)) {
    cli::cli_abort(c("`...` could not be flattened",
                   "i" = "`...` must contain only scalar individual arguments or a single flat vector/list"))
  }

  if(!is.null(.ptype)) dots <- as_vec(dots, .ptype = .ptype)
  else dots <- as.list(dots)

  return(dots)
}



