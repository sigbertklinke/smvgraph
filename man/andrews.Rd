% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/andrews.R
\name{andrews}
\alias{andrews}
\title{andrews}
\usage{
andrews(x, type = 1, step = 100, ..., normalize = 1, ymax = NA)
}
\arguments{
\item{x}{data frame or matrix}

\item{type}{type of curve (default: \code{1})
\itemize{
\item 1: \eqn{f(t)=x1/(2^0.5)+x2*sin(t)+x3*cos(t)+x4*sin(2*t)+x5*cos(2*t)+...}
\item 2: \eqn{f(t)=x1*sin(t)+x2*cos(t)+x3*sin(2*t)+x4*cos(2*t)+...}
\item 3: \eqn{f(t)=x1*cos(t)+x2*cos((2*t)^0.5)+x3*cos((3*t)^0.5)+...}
\item 4: \eqn{f(t)=1/(2^0.5)*(x1+x2*(sin(t)+cos(t))+x3*(sin(t)-cos(t))+x4*(sin(2*t)+cos(2*t))+x5*(sin(2*t)-cos(2*t))+...)}
}}

\item{step}{smoothness of curves}

\item{...}{further parameters given to \link[graphics:plot.default]{graphics::plot} and \link[graphics:lines]{graphics::lines}}

\item{normalize}{integer: normalization method (default: \code{1})
\itemize{
\item 0: no rescaling
\item 1: \eqn{(x-min(x))/(max(x)-min(x))}
\item 2: \eqn{(x-mean(x))/sd(x)}
}}

\item{ymax}{numeric: maximum of y coordinate (default: \code{NA})}
}
\value{
nothing
}
\description{
Andrews curves for visualization of multidimensional data.
\code{step} determines the number of line segments for each curve.
If \code{ymax==NA} then the maximum y coordinate will be determined from the curves.
Note that for \code{type==3} the x range is \eqn{[0, 4*pi]} otherwise \eqn{[-pi, pi]}.
Observations containing \code{NA}, \code{Nan}, \code{-Inf}, or \code{+Inf} will be deleted before plotting
}
\examples{
andrews(iris[,-5], col=as.factor(iris[,5]))
andrews(iris[,-5], type=4, col=as.factor(iris[,5]), ymax=2)
}
\references{
\itemize{
\item Andrews, D. F. (1972) Plots of High-Dimensional Data. Biometrics, vol. 28, no. 1, pp. 125-136.
\item Khattree, R., Naik, D. N. (2002) Andrews Plots for Multivariate Data: Some New Suggestions and Applications. Journal of Statistical Planning and Inference, vol. 100, no. 2, pp. 411-425.
}
}
\seealso{
In package \code{\link[andrews]{andrews}} or at \href{https://CRAN.R-project.org/package=andrews}{CRAN}
}
