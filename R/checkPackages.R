#' @rdname checkPackages
#' @aliases installPackages
#' @title checkPackages, installPackages
#' @description Checks if a package is installed without loading it. Returns a logical vector with \code{TRUE} or \code{FALSE} for each package checked.
#'
#' @param ... character: name(s) of package
#' @param add character: names of default packages to check (default: \code{c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "DT")})
#' @param plotmodule character: name(s) of plot modules to check for packages 
#'
#' @return \code{TRUE} if successful otherweise an error will be thrown
#' @export
#'
#' @examples
#' checkPackages("graphics", add=NULL)          # checks if 'graphics' is installed
#' if (interactive()) checkPackages("graphics") # checks if 'graphics', 'shiny', ... are installed
#' if (interactive()) installPackages()         # installs all packages to show ALL plots
checkPackages <- function(..., plotmodule=NULL,
                          add=c("tools", "devtools",  "formatR", "highlight", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "DT", "sortable", "base64enc", ) 
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
  ret
}

#' @rdname checkPackages
#' @export
installPackages <- function(plotmodule=NULL, add=c("highlight", "formatR", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "DT", "sortable", "base64enc", "devtools")) {
  pkgs <- add
  if (is.null(plotmodule)) {
    pkgs <- c(pkgs, unlist(lapply(module, ']]', 'packages')))
  } else {
    for (p in plotmodule) pkgs <- c(pkgs, module[[p]]$packages)
  }
  pkgs  <- unique(pkgs) 
  bpkgs <- basename(pkgs)
  for (i in seq_along(bpkgs)) {
    if (!require(bpkgs[i], character.only = TRUE)) {
      if (bpkgs[i]==pkgs[i]) utils::install.packages(pkgs[i]) else devtools::install_github(pkgs[i])
    } 
  }
}
