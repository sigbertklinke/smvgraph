library("tools")
d <- packageVersion("smvgraph")

library("htmltools")
library("smvgraph")
data <- data.frame(A1=runif(10), A2=runif(10), A3=runif(10), A4=runif(10),# A5=runif(10),
                   G1=sample(2, 10, TRUE), G2=sample(2, 10, TRUE), G3=sample(2, 10, TRUE), G4=sample(2, 10, TRUE))#, G5=sample(2, 10, TRUE))
dvar        <- getVariableInfo(data)

plotmodule  <- getModules('plot_*.R', path='.')
plots <- vector("list", 25)
dim(plots) <- c(5,5)
for (ai in 0:4) {
  for (aj in 0:4) {
    pij <- NULL
    analysis <- if (ai==0) dvar[0,,drop=FALSE] else dvar[1:ai,,drop=FALSE]
    group    <- if (aj==0) dvar[0,,drop=FALSE] else dvar[5:(4+aj),,drop=FALSE]
    for (k in seq_along(plotmodule)) {
      if (plotmodule[[k]]$usable(analysis, group, data, NULL)) pij <- c(pij, plotmodule[[k]]$label) 
    }
    plots[ai+1,aj+1] <- list(pij)
  }
}
plots[2,1] <- list(c(plots[[2,1]], "Time series"))

pkgs <- NULL
for (i in seq_along(plotmodule)) pkgs <- c(pkgs, plotmodule[[i]]$packages)
pkgs <- c(pkgs, "smvgraph", "graphics")
pkgs <- sort(basename(unique(pkgs)))
authors <- c('<table width="100%"><caption style="text-align:center"><h2>Plots and algorithms used from</h2></caption>', 
             '<tr><th style="background-color:grey;">Package</th><th style="background-color:grey;">Author(s)</th></tr>')
for (i in seq_along(pkgs)) {
  bgcolor <- if(i%%2) "white" else "lightgrey" 
  pdesc   <- packageDescription(pkgs[i])
  authi   <- htmlEscape(gsub("\n", "", pdesc$Author, fixed=TRUE))
  authors <- c(authors, paste0('<tr style="vertical-align:top;background-color:', bgcolor, '"><td><a href="https://CRAN.R-project.org/package=', pkgs[i], '">', pkgs[i], "</a></td><td>", 
                               authi, "</td></tr>"))
}
authors <- c(authors, "</table>")

labels <- unique(sapply(plotmodule, function(e) { e$label }))
setdiff(labels, unique(unlist(plots)))

html <- sprintf('<table><caption style="text-align:center"><h2>smvgraph %s</h2></caption>', d)
html <- paste0(html, '<tr><th style="background-color:grey;text-align:center;vertical-align:bottom" rowspan="2">#Analysis</th><th style="background-color:grey;text-align:center" colspan="5">#Grouping</th></tr>')
html <- paste0(html, '<tr>', paste0('<th style="background-color:grey;text-align:center">', c(0:3, "4+"), '</th>', collapse=""), '</tr>')

for (ai in 1:4) {
  for (aj in 0:4) { 
    if (length(plots[[ai+1,aj+1]])) { 
      plots[[ai+1,aj+1]] <- paste(sort(unique(trimws(gsub("\\(.*\\)", "", plots[[ai+1,aj+1]])))), collapse=", ")
    }
  }
}


g <- c(1:3, "4+")
for (ai in 1:4) {
  hj <- paste0('<tr><th style="background-color:grey;text-align:center">', g[ai], '</th>')
  aj <- 0
  while(aj<5) {
    bgcolor <- if((ai+aj)%%2) "white" else "lightgrey" 
    if (length(plots[[ai+1,aj+1]])) { 
      cs <- 1
      if (aj<4) {
        for (j in (aj+1):4) {
          if (plots[[ai+1,aj+1]]==plots[ai+1,j+1]) cs <- cs+1 else break
        }
      }
      hj <- paste0(hj, '<td style="padding:5px;background-color:', bgcolor, '" colspan="', cs, '">', plots[[ai+1,aj+1]], '</td>')
      aj <- aj+cs
    }
  }
  html <- paste0(html, hj, '</tr>')
}
html <- paste0(html, '<tr><td style="padding:5px" colspan="6"><b>Note: The availability of a plot depends on the required libraries (see "Log") and the number of unique values in a variable!</b></td></tr></table>')

writeLines(c(html, authors), "www/toc.html")