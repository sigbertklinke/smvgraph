
Rd2list <- function(Rd){
  names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"),2);
  temp_args <- Rd$arguments;
  browser()

    
  Rd$arguments <- NULL;
  myrd <- lapply(Rd, unlist);
  myrd <- lapply(myrd, paste, collapse="");
  temp_args <- temp_args[sapply(temp_args , attr, "Rd_tag") == "\\item"];
  temp_args <- lapply(temp_args, lapply, paste, collapse="");
  temp_args <- lapply(temp_args, "names<-", c("arg", "description"));
  myrd$arguments <- temp_args;
  return(myrd);
}

getHelpList <- function(...){
  thefile <- help(...)
  myrd <- utils:::.getHelpFile(thefile);
  Rd2list(myrd);
}


writeDummy <- function(pkgfuns, file="huglawurza.R") {
  pkglist <- strsplit(pkgfuns, '::')
  pkgs    <- unique(unlist(lapply(pkglist, '[[', 1)))
  browser()
  import  <- paste0("#' @import ", pkgs, collapse="\n")
  write("#' huglawurza\n#' @description A dummy function for importing all packages.\n#' @return nothing", file=file, append=FALSE)
  write(import, file=file, append=TRUE)        
  write("#' @export\n#' @examples\n#' huglawurza()\nhuglawurza <- function() {\n", file=file, append=TRUE)  
  lapply(pkglist, function(e) {
    write("if (FALSE) {", file=file, append=TRUE) 
    help <- getHelpList(e[2], package=e[1])$examples
    browser()
    write(help, file=file, append=TRUE)    
    write("}", file=file, append=TRUE) 
  })
  write("}", file=file, append=TRUE) 
}

library("smvgraph")
plotmodule  <- getModules('plot_*.R', path='.')
help <- NULL
for (i in 1:length(plotmodule)) help <- c(help, plotmodule[[i]]$help)
writeDummy(help)
