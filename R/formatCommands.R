#' formatCommands
#'
#' @param cmds characater: R code
#'
#' @return HTML code for the splot app
#' @export
#'
#' @examples
#' formatCommands('print("Hello World!")')
formatCommands <- function(cmds) {
  tmpfile <- tempfile(fileext=".R")
  on.exit(unlink(tmpfile))
  #
  cmds     <- unlist(strsplit(cmds, "\n"))
  blanks   <- gsub("\\S.*$", "", cmds)
  Encoding(cmds) <- "UTF-8"
  #
  err <- try({
    fmtsrc   <- formatR::tidy_source (text=cmds,
                                      width.cutoff=100-max(nchar(blanks)),
                                      output=FALSE)
    writeLines(fmtsrc$text.tidy, tmpfile)
    src <- highlight::highlight(tmpfile, output=NULL, renderer = highlight::renderer_html(document=FALSE),
                                showPrompts = TRUE, prompt="", continue="")
  }, silent=TRUE)
  if ("try-error" %in% class(err)) src <- paste0('<pre style="color:red;">', as.character(err), '<pre>')
  src
}