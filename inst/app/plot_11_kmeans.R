module[["kmeans_plot"]] <- list(
  label    = "K-Means clustering",
  help     = "graphics::kmeans",
  packages = "psych",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && isTRUE(all(analysis$unique>1)) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
0:        library('psych')
0:        x  <- numeric_data(data, select={{x}})
0:        keep <- is.finite(rowSums(x))
0:        x <- x[keep,]
!1:       x  <- scale(x)
0:        pc <- principal(x, 2, rotate='none', covar=TRUE)
0:        km_cl <- kmeans(x, {{n}})
0:        col <- hcl.colors(max(km_cl$cluster))[km_cl$cluster]
0:        plot(pc$scores, col=col, pch=19)
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             n=getval(input$kmeans_plot_n, 2),
             rep=getval(input$kmeans_plot_repeat, -1),
             getval(input$kmeans_plot_covar, FALSE)     #1                   
             )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("kmeans_plot_covar", "Unstandardized data"),
         sliderInput("kmeans_plot_n", "Number of clusters", 2, 15, 2, 1),
         actionButton("kmeans_plot_repeat", "Cluster again")
         )
  }
)
