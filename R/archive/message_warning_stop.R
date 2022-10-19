
message <- function(..., domain = NULL, appendLF = TRUE) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "INFO", log_msg = args)
  base::message(unlist(args), domain = domain, appendLF = appendLF)
}


warning <- function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,  domain = NULL) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "WARN", log_msg = args[[1]])
  base::warning(unlist(args), call. = call., immediate. = immediate., noBreaks. = noBreaks., domain = domain)
}


stop <- function(..., call. = TRUE, domain = NULL) {
  args <- paste(list(...), collapse = "")
  loggit(log_lvl = "ERROR", log_msg = args[[1]])
  base::stop(unlist(args), call. = call., domain = domain)
}