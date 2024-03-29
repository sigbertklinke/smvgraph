% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_andrews.R
\name{order_andrews}
\alias{order_andrews}
\title{order_andrews}
\usage{
order_andrews(x, method = 1)
}
\arguments{
\item{x}{data matrix}

\item{method}{numeric: order method (default: \code{1})
\itemize{
\item 1: interquartile range
\item 2: \eqn{max(x-median(x))/IQR(x)} (outlier)
\item 3: fit to a Ward cluster solution with euclidean distance
}}
}
\value{
order of column vectors
}
\description{
Returns a reording of the columns of \code{x} to visualize outliers or clusters better.
If no colum names are given then \code{V1}, \code{V2}, ... will be used.
}
\examples{
order_andrews(iris)
}
