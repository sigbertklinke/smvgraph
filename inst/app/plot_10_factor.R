module[["factor_plot"]] <- list(
  label = "Factor analysis (psych)",
  help  = "psych::fa",
  packages= c("psych", "devtools", "plot.matrix"),
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && isTRUE(all(analysis$unique>1)) 
  },
  code = function(analysis, group, data, input) {
    template("
    0:        library('psych')
    0:        x <- numeric_data(data, select={{x}})
    0:        keep <- is.finite(rowSums(x))
    0:        x <- x[keep,]
    6:        col <- color_data(data, select={{g}})[keep]
    !6:       col <- 'black'
    !1:       f <- try(fa(x, nfactors={{n}}, rotate={{rot}}, fm={{fm}}, covar={{covar}}), silent=TRUE)
    !1:       if ('try-error' %in% class(f)) stop('extraction/rotation method does not work, did you use Principal axis?')
    1:        f <- principal(x, nfactors={{n}}, rotate={{rot}}, covar={{covar}})
    0:        layout(matrix(c(1,1,2), nrow=1))
    2&!6:     boxplot(f$scores)
    2&6:      grp <- names(col)
    2&6:      boxplot(f$scores~grp)
    2:        rug(f$scores, side=2)
    !2&4:     parcoord(f$scores, col=col)
    !2&!4&3:  plot.default(f$scores, col=col, pch=19)
    !2&!4&!3: pairs(f$Scores, col=col, pch=19)
    5:        scree(x) # uses always correlation!
    !5:       library('plot.matrix')
    !5:       opar <- par(mar=c(5.1, 4.1, 4.1, 4.1))
    !5:       plot(f$loadings)
    !5:       par(opar)
    !5:       devtools::unload('plot.matrix')
    0:        layout(1)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=sprintf("c(%s)", paste0("'", row.names(group), "'", collapse=', ')),
             n=getval(input$factor_plot_nfactors, 1),
             covar=getval(input$factor_plot_covar, FALSE),
             rot=txt(getval(input$factor_plot_rotate, "none")), 
             fm=txt(getval(input$factor_plot_fm, "pc")), 
             getval(input$factor_plot_fm, "pc")=="pc",  #1
             getval(input$factor_plot_nfactors, 1)==1,  #2 only one factor extracted
             getval(input$factor_plot_nfactors, 1)==2,  #3 exactly two factors extracted
             getval(input$factor_plot_plot, 1)==1,      #4 plot_plot = parcoord
             getval(input$factor_plot_subplot, 1)==1,   #5 _plot_subplot = scree plot
             nrow(group)>0                              #6 color by groups
             )
  },
  ui = function(analysis, group, data, input) {
   list(checkboxInput("factor_plot_covar", "Unstandardized data"),
        sliderInput("factor_plot_nfactors", "Factors to extract", 1, nrow(analysis), 1, 1),
        selectInput("factor_plot_fm", "Extraction method",
                    choices=c("Principal component"="pc", "Principal axis"="pa", "Maximum-Likelihood"="ml", "Minimum residual/ULS"="minres")),
        selectInput("factor_plot_rotate", "Rotation method",
                    choices=toChoice(NA, "none", "varimax", "promax", "oblimin")),
        selectInput("factor_plot_plot", "Plot type",
                    choices=toChoice(1, "Parallel coordinate", "Scatter plot matrix")),
        selectInput("factor_plot_subplot", "Subplot type",
                    choices=toChoice(1, "Scree plot", "Loadings"))

        ) 
  }
)