#' prepare_data
#'
#' Prepares a matrix or data frame:
#' 
#' * extracts all numeric variables or columns,
#' * deletes non-complete observations, and
#' * creates column titles if they do not exist.
#'
#' @param data matrix or data.frame
#' @param title character: prefix title for columns if they do not exist
#' @param shorten integer: should the title be shortened (default: `TRUE`)
#'
#' @md
#' @return matrix
#' @export
#'
#' @examples
#' prepare_data(iris[,-5], "iris[,-5]")
prepare_data <- function(data, title, shorten=TRUE) {
  if (is.data.frame(data)) data <- as.matrix(data[,sapply(data, is.numeric)])
  stopifnot("matrix" %in% class(data))
  data <- data[stats::complete.cases(data),]
  stopifnot(nrow(data)>1)
  if (shorten) title <- gsub("[^[:alnum:]]", "", title)
  if (is.null(colnames(data))) colnames(data) <- sprintf("%s[,%.0f]", title, 1:ncol(data))
  data
}