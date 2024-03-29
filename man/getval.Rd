% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getval.R
\name{getval}
\alias{getval}
\title{getval}
\usage{
getval(val, ...)
}
\arguments{
\item{val}{current value}

\item{...}{sequence of alternative values}
}
\value{
a value
}
\description{
Returns \code{val} if \code{length(val)>1}. Otherwise it runs through \code{args=list(...)} until it finds an element with
\code{length(args[[i]])>0} and returns it. If everything fails \code{NULL} will be returned.
}
\examples{
getval(NULL, 0)
getval(1, 0)
}
