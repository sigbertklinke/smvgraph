module[["dbscan_plot"]] <- list(
  label    = "DBSCAN clustering",
  help     = "dbscan::dbscan",
  packages = c("psych", "dbscan"),
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && isTRUE(all(analysis$unique>1)) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    x       <- data[,row.names(analysis)]
    x       <- x[is.finite(rowSums(x)),]
    maxdist <- max(quantile(dist(x), 0.5), quantile(dist(scale(x)), 0.5))
    maxdist <- round(maxdist, 2-log10(maxdist))
    template("
0:        library('psych')
0:        library('dbscan')
0:        x  <- numeric_data(data, select={{x}})
0:        keep <- is.finite(rowSums(x))
0:        x <- x[keep,]
!1:       x  <- scale(x)
0:        pc <- principal(x, 2, rotate='none', covar=TRUE)
0:        db_cl <- dbscan(x, eps={{eps}}, minPts={{pts}})
0:        cpal  <- c('grey',  hcl.colors(max(db_cl$cluster)))
0:        plot(pc$scores, col=cpal[1+db_cl$cluster], pch=19)
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             eps=getval(input$dbscan_plot_eps, maxdist/3),
             pts=getval(input$dbscan_plot_pts, 2, 10, 5, 1),
             getval(input$dbscan_plot_covar, FALSE) #1                        
             )
  },
  ui = function(analysis, group, data, input) {
    x       <- data[,row.names(analysis)]
    x       <- x[is.finite(rowSums(x)),]
    maxdist <- max(quantile(dist(x), 0.5), quantile(dist(scale(x)), 0.5))
    maxdist <- round(maxdist, 2-log10(maxdist))
    list(checkboxInput("dbscan_plot_covar", "Unstandardized data"),
         sliderInput("dbscan_plot_eps", "Core distance", min=0, max=maxdist, value = maxdist/3),
         sliderInput("dbscan_plot_pts", "Minimal neighbours", min=2, max=10, value = 5)
         )
  }
)
