#' variable_bucket_list
#'
#' Creates a `bucket_list` for a variable choice in Shiny app.
#'
#' @param data matrix: data
#' @param xvar character: names of variables to use
#' @param fun function: for returning a list of variable names (default: `NULL`)
#'
#' @return character with variable names
#' @export
#'
#' @examples
#' # none yet
variable_bucket_list <- function(data, xvar, fun=NULL) {
  dvar <- dimnames(data)[[2]]
  if (length(xvar)) {
    xvar <- xvar[xvar %in% dvar]
    dvar <- setdiff(dvar, xvar)
  } else {
    xvar <- if (is.null(fun)) dvar else fun(data)
    dvar <- NULL
  }
  bucket_list(
    header = NULL,
    group_name = "bucket_var_group",
    orientation = "vertical",
    class = c("default-sortable", "black-text"),
    add_rank_list(
      text = "Variable(s)",
      labels = dvar,
      input_id = "dvar"
    ),
    add_rank_list(
      text = "Selected variable(s)",
      labels = xvar,
      input_id = "xvar"
    )
  )
}