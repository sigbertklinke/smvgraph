module[["distance_plot"]] <- list(
  label    = "Data distances",
  help     = "stats::dist",
  packages = "psych",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
0:        library('psych')
0:        x   <- numeric_data(data, select={{x}})
0:        keep <- is.finite(rowSums(x))
0:        x   <- x[keep,]
!1:       x   <- as.data.frame(scale(x))
0:        tab <- matrix(0, ncol=6, nrow=length(x))
0:        dij <- as.numeric(abs(x[{{left}},]-x[{{right}},]))
0:        tab[,1] <- diag(var(x))                                  # Total variance
0:        tab[,1] <- tab[,1]/sum(tab[,1])                          
0:        tab[,2] <- seq(dij)==which.min(dij)                      # Minimum
0:        tab[,3] <- dij/sum(dij)                                  # Manhattan
0:        gij     <- dij/sapply(x, function(v) { diff(range(v)) }) # Gower
0:        tab[,4] <- gij/sum(gij)
0:        tab[,5] <- dij^2/sum(dij^2)                              # Euclidean
0:        tab[,6] <- seq(dij)==which.max(dij)                      # Maximum
0:        colnames(tab) <- c('Total\\nVariance', 'Minimum', 'Manhattan', 'Gower', 'Euclidean', 'Maximum')
0:        opar <- par(mar=c(5.1, 4.1*max(nchar(names(x)))/7, 4.1, 2.1))
0:        barplot(tab, axes=FALSE, col=hcl.colors(length(x)), cex.names=0.8, sub='Observation pair ({{left}}, {{right}})')
0:        axis(2, at=cumsum(tab[,1])-tab[,1]/2, labels = names(x), las=2)
0:        axis(4, at=(0:5)/5, labels=sprintf('%.0f%%', (0:5)*20))
0:        par(opar)
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             left=getval(input$smvgraph_obs[1], 1),
             right=getval(input$smvgraph_obs[2], sum(is.finite(rowSums(data[,row.names(analysis)])))),
             getval(input$distance_plot_covar, TRUE) #1                        
             )
  },
  ui = function(analysis, group, data, input) {
    x       <- data[,row.names(analysis)]
    list(checkboxInput("distance_plot_covar", "Unstandardized data", TRUE),
         UIobservations(sum(is.finite(rowSums(x))))
         )
  }
)
