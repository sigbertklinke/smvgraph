module[["hclust_plot"]] <- list(
  label    = "Hierarchical clustering",
  help     = "stats::hclust",
  packages = "psych",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && isTRUE(all(analysis$unique>1)) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
0:        library('psych')
0:        x  <- numeric_data(data, select={{x}})
0:        keep <- is.finite(rowSums(x))
0:        x  <- x[keep,]
!1:       x  <- scale(x)
0:        pc <- principal(x, 2, rotate='none', covar=TRUE)
0:        d  <- dist(x, {{dist}})
0:        hc_cl <- hclust(d, {{method}})
0:        colcl <- hcl.colors({{n}})
0:        clt   <- cutree(hc_cl, {{n}})
0:        layout(mat = matrix(c(1,1,2,1,1,3), ncol=2))
0:        plot(pc$scores, col=colcl[clt], pch=19)
0:        # dendrogram
0:        rheight <- rev(hc_cl$height)
0:        rh <- (rheight[{{n}}-1]+rheight[{{n}}])/2
0:        plot(hc_cl, hang=-1, sub='', labels=FALSE, ylim=c(0, max(hc_cl$height)))
0:        abline(h=rh, col='blue')
0:        plot(1:15, rheight[1:15], pch=19, type='b', ylim=c(0, max(hc_cl$height)), xlim=c(1,15), xlab='Cluster', ylab='Height')
0:        abline(h=rh, col='blue')
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             n=getval(input$hclust_plot_n, 2),
             dist=txt(getval(input$hclust_plot_dist, "euclidean")),
             method=txt(getval(input$hclust_plot_method, "ward.D")),
             getval(input$hclust_plot_covar, FALSE) #1                        
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("hclust_plot_covar", "Unstandardized data"),
         sliderInput("hclust_plot_n", "Number of clusters", 2, 15, 2, 1),
         splitLayout(cellWidths = c("50%", "50%"),
                     selectInput("hclust_plot_dist", "Distance", size=4, selectize=FALSE,
                                  choices = toChoice(NA, "euclidean", "maximum", "manhattan", "canberra")),
                     selectInput("hclust_plot_method", "Method", size=4, selectize=FALSE,
                                  choices = toChoice(NA, "ward.D", "ward.D2", "single", "complete", "average", 
                                                         "mcquitty", "median", "centroid")))
    )
  }
)
