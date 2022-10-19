#' loggit
#'
#' Stores in a temporary file the log messages including messages, warnings and errors. 
#'
#' @param log_lvl character: Level of log output. In actual practice, one of "DEBUG", "INFO", "WARN", and "ERROR" are common, but any string may be supplied
#' @param log_msg character: Main log message
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
  if (is.null(smvgraph_env$logfile)) {
    set_logfile()
    loggit('DEBUG', 'Logfile in loggit created')
  }
  smvgraph_env$loglist$log_time <- c(smvgraph_env$loglist$log_time, Sys.time())
  smvgraph_env$loglist$log_lvl  <- c(smvgraph_env$loglist$log_lvl, log_lvl)
  smvgraph_env$loglist$log_msg  <- c(smvgraph_env$loglist$log_msg, log_msg)  
  saveRDS(smvgraph_env$loglist, smvgraph_env$logfile)
}

#' @rdname loggit
#' @export
read_logs <- function() {
  if (is.null(smvgraph_env$logfile)) {
    set_logfile()
    loggit('DEBUG', 'Logfile in read_logs created')
  }
  as.data.frame(readRDS(smvgraph_env$logfile))
}

#' @rdname loggit
#' @export
set_logfile <- function() {
  smvgraph_env$logfile <- tempfile(pattern = "loggit", fileext = ".rds", tmpdir = tempdir(TRUE))
  smvgraph_env$loglist <- list(log_time=Sys.time(), log_lvl="DEBUG", log_msg=sprintf("Created logfile %s", smvgraph_env$logfile))
}
