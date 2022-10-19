module <- new.env()
smvgraph_env <- new.env()

.onLoad <- function(libname, pkgname){
  files  <- Sys.glob(file.path(system.file("app", package="smvgraph"), "plot_*.R"))
  for (file in files) try(eval(parse(file)), silent=TRUE)
}

.onAttach <- function(libname, pkgname){
  if (interactive()) {
     pkgs <- c("tools", "devtools",  "formatR", "highlight", "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "DT", "sortable", "base64enc")
     ret  <- structure(rep(NA, length(pkgs)), names=pkgs)
     for (pkg in pkgs) ret[pkg] <- !nzchar(system.file(package=pkg))
     if(any(ret)) {
        msg <- sprintf("Packages(s) not yet installed: %s\n", paste0(names(ret)[ret], collapse=', ')) 
        packageStartupMessage(msg, "To finish the installation call 'installPackages()' once! You may read the vignette 'vignette(\"smvgraph\")', too.")
     }
  }
}

#' zzz
#'
#' Runs all `s...` functions for test purposes if interactively called.
#'
#' @return nothing
#' @importFrom stats runif
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinyWidgets progressBar
#' @rawNamespace import(shinydashboardPlus, except=progressBar)
#' @import DT 
#' @import sortable 
#' @import base64enc
#' @export
#'
#' @examples
#' zzz()
zzz <- function() {
  if (interactive()) {
#    library("smvgraph")
    sandrews(datasets::iris)
    sandrews(normalize(datasets::iris, 0))
    schernoff(datasets::iris)
    schernoff(normalize(datasets::iris, 0))
    spairs(datasets::iris)
    spairs(normalize(datasets::iris, 0))
    sparcoord(datasets::iris)
    sparcoord(normalize(datasets::iris, 0))
    sradar(datasets::iris)
    sradar(normalize(datasets::iris, 0))
    m <- matrix(runif(36), ncol=4)
    sandrews(m)
    schernoff(m)
    spairs(m)
    sparcoord(m)
    sradar(m)
    #
    smosaic(datasets::Titanic)
    smosaic(toDataframe(datasets::Titanic))
    m <- matrix(sample(1:6, size=36, replace = TRUE), ncol=4)
    smosaic(m)
    t <- datasets::Titanic
    dimnames(t) <- NULL
    smosaic(t)
    #
    sdistance(datasets::iris)
    #
    sdbscan(datasets::iris)
    shclust(datasets::iris)
    skmeans(datasets::iris)
    smclust(datasets::iris)
  }
}

