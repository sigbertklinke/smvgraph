library("tools")
d <- packageDescription("smvgraph")

library("smvgraph")
data <- data.frame(A1=runif(10), A2=runif(10), A3=runif(10), A4=runif(10),# A5=runif(10),
                   G1=sample(2, 10, T), G2=sample(2, 10, T), G3=sample(2, 10, T), G4=sample(2, 10, T))#, G5=sample(2, 10, T))
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


labels <- unique(sapply(plotmodule, function(e) { e$label }))
setdiff(labels, unique(unlist(plots)))

png("www/wordcloud.png", width=800, height=600)
plot(c(0.5,4.5), c(-0.5,4.5), type="n", ylab="Number of Grouping variables", xlab="Number of Analysis variables",
     main=paste("smvgraph", d$Version), sub="Note: The availablity of a plot depends on the required libraries and the number of unique values!")
width <- 28
for (ai in 1:4) {
  for (aj in 0:4) {
    if (length(plots[[ai+1,aj+1]])) {
      if((ai+aj)%%2) polygon(c(ai-0.5, ai+0.5, ai+0.5, ai-0.5), c(aj-0.5, aj-0.5, aj+0.5, aj+0.5), col="grey", border=NA)
      plt  <- sort(unique(gsub("(.*)", "", plots[[ai+1,aj+1]]))))
      len  <- nchar(plt)
      line <- 1+(cumsum(len)%/%width)
      txt  <- rep(NA_character_, max(line))
      for (i in 1:max(line)) {
        txt[i] <- paste0(plt[line==i], collapse=", ")
      }
      txt <- paste0(txt, collapse="\n")
      text(ai, aj, txt, cex=0.675)
    }
  }
}
dev.off()

html <- sprintf('<table><caption><h2>smvgraph %s</h2></caption><tr><th style="background-color:grey">#Analysis<br><br>#Grouping</th>', d$Version)
html <- paste0(html, paste0('<th style="background-color:grey">', 1:4, '</th>', collapse=""), '</tr>')

for (aj in 0:4) { 
  hj <- paste0('<tr><th style="background-color:grey">', aj, '</th>')
  for (ai in 1:4) {
    if (length(plots[[ai+1,aj+1]])) { 
      bgcolor <- if((ai+aj)%%2) "white" else "lightgrey" 
      plt <- sort(unique(trimws(gsub("\\(.*\\)", "", plots[[ai+1,aj+1]]))))
      hj <- paste0(hj, '<td style="padding:5px;background-color:', bgcolor, '">', paste0(plt, collapse=", "), '</td>')
    }
  }
  html <- paste0(html, hj, '</tr>')
}
html <- paste0(html, '</table>')

writeLines(html, "www/toc.html")