smvgraph_env <- new.env()
#' loggit
#'
#' Stores in a temporary file the log messages including messages, warnings and errors. 
#'
#' @param log_lvl character: Level of log output. In actual practice, one of "DEBUG", "INFO", "WARN", and "ERROR" are common, but any string may be supplied
#' @param log_msg character: Main log message
#' @inheritParams base::stop 
#' @inheritParams base::warning 
#' @inheritParams base::message
#' @aliases stop warning message read_logs
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   set_logfile()      # create a temporary file for logging
#'   loggit("DEBUG", "Hello world")
#'   read_logs()        # get a data frame with the current messages.
#' }
loggit <- function(log_lvl, log_msg) {
  smvgraph_env$loglist$log_time <- c(smvgraph_env$loglist$log_time, Sys.time())
  smvgraph_env$loglist$log_lvl  <- c(smvgraph_env$loglist$log_lvl, log_lvl)
  smvgraph_env$loglist$log_msg  <- c(smvgraph_env$loglist$log_msg, log_msg)  
  saveRDS(smvgraph_env$loglist, smvgraph_env$logfile)
}

#' @rdname loggit
#' @export
read_logs <- function() {
  as.data.frame(readRDS(smvgraph_env$logfile))
}

#' @rdname loggit
#' @export
set_logfile <- function() {
  smvgraph_env$logfile <- tempfile(pattern = "loggit", fileext = ".rds")
  smvgraph_env$loglist <- list(log_time=Sys.time(), log_lvl="DEBUG", log_msg=sprintf("Created logfile %s", smvgraph_env$logfile))
}

#' @rdname loggit
#' @export
message <- function(..., domain = NULL, appendLF = TRUE) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "INFO", log_msg = args)
  base::message(unlist(args), domain = domain, appendLF = appendLF)
}

#' @rdname loggit
#' @export
warning <- function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,  domain = NULL) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "WARN", log_msg = args[[1]])
  base::warning(unlist(args), call. = call., immediate. = immediate., noBreaks. = noBreaks., domain = domain)
}

#' @rdname loggit
#' @export
stop <- function(..., call. = TRUE, domain = NULL) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "ERROR", log_msg = args[[1]])
  base::stop(unlist(args), call. = call., domain = domain)
}