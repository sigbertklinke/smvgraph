#' @rdname checkPackages
#' @aliases installPackages
#' @title checkPackages, installPackages
#' @description Checks if a package is installed without loading it. Returns a logical vector with \code{TRUE} or \code{FALSE} for each package checked.
#'
#' @param ... character: name(s) of package
#' @param add character: names of default packages to check (default: \code{c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "DT")})
#' @param plotmodule character: name(s) of plot modules to check for packages 
#' @param error logical: should a error thrown if one or more package are missing? (default: \code{FALSE})
#'
#' @return \code{TRUE} if successful otherweise an error will be thrown
#' @export
#'
#' @examples
#' checkPackages("graphics", add=NULL)          # checks if 'graphics' is installed
#' if (interactive()) checkPackages("graphics") # checks if 'graphics', 'shiny', ... are installed
#' if (interactive()) installPackages()         # installs all packages to show ALL plots
checkPackages <- function(..., plotmodule=NULL,
                          add=c("tools", "devtools",  "formatR", "highlight", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "DT", "sortable", "base64enc"),
                          error=FALSE
                         ) {
  pkgs <- as.character(unlist(list(...)))
  if (length(add)) pkgs <- c(pkgs, as.character(add))
  if (length(plotmodule)) {
    for (p in plotmodule) pkgs <- c(pkgs, module[[p]]$packages)
  }
  pkgs <- basename(unique(pkgs))
  ret  <- structure(rep(NA, length(pkgs)), names=pkgs)
  for (pkg in pkgs) {
    ret[pkg] <- nzchar(system.file(package=pkg))
  }
  if(error && !all(ret)) {
    msg <- sprintf("%i packages(s) are missing, please install with 'installPackages(%s)'", sum(!ret), 
                   if (length(plotmodule)) paste0(sQuote(plotmodule), collapse=", ") else "")
    stop(msg)
  }
  ret
}

#' @rdname checkPackages
#' @export
installPackages <- function(plotmodule=NULL, add=c("tools", "devtools",  "formatR", "highlight", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "DT", "sortable", "base64enc"))  {
  pkgs <- add
  if (!is.null(plotmodule)) {
    for (p in plotmodule) pkgs <- c(pkgs, module[[p]]$packages)
  }
  pkgs  <- unique(pkgs) 
  if (interactive()) cat("Packages to install: ", paste0(pkgs, collapse=','), "\n   This may take a while....\n\n")
  bpkgs <- basename(pkgs)
  for (i in seq_along(bpkgs)) {
    if (!require(bpkgs[i], character.only = TRUE)) {
      if (bpkgs[i]==pkgs[i]) utils::install.packages(pkgs[i]) else devtools::install_github(pkgs[i])
    } 
  }
}
