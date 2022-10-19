testPlots <- function(data=NULL) {
  opar <- par(no.readonly = TRUE)
#  warn <- options(warn=2)
#  on.exit({
#    par(opar)
#    options(warn)
#  })
  if (is.null(data)) {
    data("testdata")
    data <- testdata
  }
  stopifnot(ncol(data)>5)
  vdata <- getVariableInfo(data)
  vdata <- vdata[order(vdata$unique, decreasing=TRUE),]
  modules <- getModules('plot_*.R')   # get plots
  cat("Test plots:\n")
  ret <- list(label=character(0), plot=character(0), analysis=integer(0), group=integer(0))
  for (plot in names(modules)) {
    cat(modules[[plot]]$source, "\n")
    cat(" ", plot, "\n")
    for (i in 1:4) {
      for (j in 0:2) {
        for (k in 0:1) {
          if (k==0) {
            aindex <- sample(ncol(testdata), i)
            gindex <- sample(ncol(testdata), j)
          }
          if (k==1) {
            aindex <- 1:i
            gindex <- switch(j+1, integer(0), nrow(vdata), nrow(vdata)-1:2)
          }
          analysis <- vdata[aindex,,drop=FALSE]
          group    <- vdata[gindex,,drop=FALSE]        
          if (modules[[plot]]$usable(analysis, group, data, NULL)) {
            cat("   ", paste0(row.names(analysis), collapse=", "), "/", paste0(row.names(group), collapse=", "), "\n") 
            env <- new.env()
            env$data <- data
            code <- try (modules[[plot]]$code(analysis, group, data, NULL))
            if ("try-error" %in% class(code)) return(list(code, analysis, group))
            suppressWarnings(par(opar))
            fail <- try(eval(parse(text=code), envir = env))
            #dev.off()
            if ("try-error" %in% class(fail)) return(list(code, analysis, group))
            ret$label    <- c(ret$label, modules[[plot]]$label)
            ret$plot     <- c(ret$plot, plot)
            ret$analysis <- c(ret$analysis, i)          
            ret$group    <- c(ret$group, j)    
            break
          }
        }
      } 
    }
  }
  as.data.frame(ret)
}

library("smvgraph")
testPlots()