#' Test data
#'
#' A data frame containing various variable types and special values. 
#'
#' @format A data frame with `n=25` rows and 8 variables:
#' \describe{
#'   \item{xu}{`runif(n)` with a `NA`, `NaN`, `Inf`, `-Inf`}
#'   \item{xn}{`rnorm(n, 0, 2)` with a `NA`, `NaN`}
#'   \item{x0}{`rep(0, n)`}
#'   \item{xi}{`as.integer(rnorm(n, 0, 2)` with a `NA`, `NaN`}
#'   \item{x2}{`sample(c(0,1), size=n, replace=TRUE)`}
#'   \item{gf}{`factor(as.integer(rnorm(n, 0, 2))` with a `NA`}
#'   \item{go}{`ordered(as.integer(rnorm(n, 0, 2))` with a new level `10`}
#'   \item{gn}{`ordered(as.integer(rnorm(n, 0, 2))` with a `NA`}
#'   \item{gc}{`as.character(as.integer(rnorm(n, 0, 2))` with a `NA` and `""`}
#'   \item{gl}{`sample(c(T,F), size=n, replace=TRUE))` with a `NA`}
#'   \item{g0}{`rep("constant, n)`}
#'   \item{g2}{`sample(c(T,F), size=n, replace=TRUE)`}
#' }
"testdata"
