% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loggit.R
\name{loggit}
\alias{loggit}
\alias{stop}
\alias{warning}
\alias{message}
\alias{read_logs}
\alias{set_logfile}
\title{loggit}
\usage{
loggit(log_lvl, log_msg)

read_logs()

set_logfile()

message(..., domain = NULL, appendLF = TRUE)

warning(
  ...,
  call. = TRUE,
  immediate. = FALSE,
  noBreaks. = FALSE,
  domain = NULL
)

stop(..., call. = TRUE, domain = NULL)
}
\arguments{
\item{log_lvl}{character: Level of log output. In actual practice, one of "DEBUG", "INFO", "WARN", and "ERROR" are common, but any string may be supplied}

\item{log_msg}{character: Main log message}

\item{...}{zero or more objects which can be coerced to character
    (and which are pasted together with no separator) or a single
    condition object.}

\item{domain}{see \code{\link[base]{gettext}}.  If \code{NA}, messages will
    not be translated.}

\item{appendLF}{logical: should messages given as a character string
    have a newline appended?}

\item{call.}{logical, indicating if the call should become part of the
    error message.}

\item{immediate.}{logical, indicating if the call should be output
    immediately, even if \code{\link[base]{getOption}("warn") <= 0}.}

\item{noBreaks.}{logical, indicating as far as possible the message should
    be output as a single line when \code{options(warn = 1)}.}
}
\value{
Nothing.
}
\description{
Stores in a temporary file the log messages including messages, warnings and errors.
}
\examples{
if (interactive()) {
  set_logfile()      # create a temporary file for logging
  loggit("DEBUG", "Hello world")
  read_logs()        # get a data frame with the current messages.
}
}
