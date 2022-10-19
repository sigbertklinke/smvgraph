module[["mclust_plot"]] <- list(
  label    = "EM clustering",
  help     = "mclust::Mclust",
  packages = c("psych", "mclust"),
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>1) && isTRUE(all(analysis$unique>1)) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
0:        library('psych')
0:        library('mclust')
0:        x  <- numeric_data(data, select={{x}})
0:        keep <- is.finite(rowSums(x))
0:        x <- x[keep,]
!1:       x  <- scale(x)
0:        pc <- prcomp(x)
0:        em_cl <- Mclust(x, G={{n}}, modelNames={{model}})
0:        col <- hcl.colors(max(em_cl$classification))[em_cl$classification]
0:        plot(pc$x[,1:2], col=col, pch=19)
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             n=getval(input$mclust_plot_n, 2),
             model=txt(getval(input$mclust_plot_model, "EII")),
             getval(input$mclust_plot_covar, FALSE)     #1                   
             )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("mclust_plot_covar", "Unstandardized data"),
         sliderInput("mclust_plot_n", "Number of clusters", 2, 15, 2, 1),
         selectInput("mclust_plot_model", "Models", choices=list(
           "ellipsoidal, varying volume, shape, and orientation (VVV)" ="VVV"   ,
           "ellipsoidal, equal volume (EVV)" ="EVV"    ,
           "ellipsoidal, equal shape (VEV)" ="VEV"    ,
           "ellipsoidal, equal volume and equal shape (EEV)" ="EEV",
           "ellipsoidal, equal orientation (VVE)" ="VVE"    ,
           "ellipsoidal, equal shape and orientation (VEE)" ="VEE",
           "ellipsoidal, equal volume and orientation (EVE)" ="EVE",
           "ellipsoidal, equal volume, shape, and orientation (EEE)" ="EEE",
           "diagonal, varying volume and shape (VVI)" ="VVI"    ,
           "diagonal, equal volume, varying shape (EVI)" ="EVI"  ,
           "diagonal, varying volume, equal shape (VEI)" ="VEI"   ,
           "diagonal, equal volume and shape (EEI)" ="EEI"    ,
           "spherical, unequal volume (VII)" ="VII"    ,
           "spherical, equal volume (EII)" ="EII"
         ))
         )
  }
)
