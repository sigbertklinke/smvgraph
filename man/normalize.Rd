% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize}
\alias{normalize}
\title{normalize}
\usage{
normalize(x, method = 1)
}
\arguments{
\item{x}{data.frame or matrix}

\item{method}{integer: normalization method (default: \code{1})
\itemize{
\item 0: no rescaling
\item 1: \eqn{(x-min(x))/(max(x)-min(x))}
\item 2: \eqn{(x-mean(x))/sd(x)}
}}
}
\value{
numeric matrix
}
\description{
Extracts the numeric vectors from a data frame and normalizes each vector.
Note: In case that a variable is constant for \code{method==1} (minmax) the entries will be replaced by \code{0.5} and
for \code{method==2} (standardization) the entries will be replaced by \code{0}.
}
\examples{
normalize(iris, 2)
}
\seealso{
In package \code{\link[andrews]{normalize}} or at \href{https://CRAN.R-project.org/package=andrews}{CRAN}
}
