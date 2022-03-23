#' getModules
#'
#' Returns a list of available module as a list. 
#'
#' @param pattern character: character string containing a regular expression, currently are used `plot_*.R` and `color_*.R`
#' @param path character: containing the path where to search the modules
#'
#' @return a list with the modules
#' @export
#'
#' @examples
#' library("shiny")
#' getModules('plot_*.R')   # get plots
#' getModules('color_*.R')  # get colors
getModules <- function(pattern, path=getShinyOption("smvgraph.path")) {
  # load plots
  #browser()
  module <- list()
  if (is.null(path)) path <- system.file("app", package="smvgraph")
  files    <- Sys.glob(file.path(path, pattern))
  files    <- files[order(tools::file_path_sans_ext(files))]
  #browser()
  maxlen  <- max(nchar(files))
  listofplot <- data.frame(file=character(0), type=character(0), msg=character(0))
  for (file in files) {
    old <- names(module)
    res <- try(eval(parse(file)), silent=TRUE)
    if ("try-error" %in% class(res)) {
      listofplot[nrow(listofplot)+1,] <- list(file=file, type=NA_character_, msg="failed to load, ignored")
    } 
    npts <- setdiff(names(module), old)
    for (i in seq_along(npts)) {
      last <- length(listofplot$msg)
      msg  <- NA_character_
      pkgs <- sapply(module[[npts[i]]]$packages, require,  quietly=TRUE, character.only = TRUE)
      if(!all(pkgs)) msg <- sprintf("missing packages: %s", paste0(names(pkgs)[!pkgs], collapse=","))
      listofplot[nrow(listofplot)+1,] <- list(file=file, type=npts[i], msg=msg) 
    }  
  }
  # errors
  #browser()
  listofplot <- listofplot[order(listofplot$msg, listofplot$file, listofplot$type),]
  delplot    <- listofplot$type[!is.na(listofplot$msg) & !is.na(listofplot$type)]
  listofplot$msg[is.na(listofplot$msg)] <- "OK"
  infotxt <- paste0(sprintf("%*s - ", -max(nchar(listofplot$file), na.rm=TRUE), listofplot$file),
                            sprintf("%*s : ", -max(nchar(listofplot$type), na.rm=TRUE),  listofplot$type), 
                            listofplot$msg)
  for (i in seq_along(delplot)) module[[delplot[i]]] <- NULL
  structure(module, infotxt=infotxt)
}
