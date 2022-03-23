module[["sunflower"]] <- list(
  label = "Sunflower plot",
  help  = "graphics::sunflowerplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    template("
             0: x  <- numeric_data(data, select={{x}}) 
             0: xb <- binData(x, bins=c({{binx}}, {{biny}}), out='binned')
             0: sunflowerplot(xb[,1:2], number=xb$Freq)
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             binx=getval(input$sunflower_xbin,10),
             biny=getval(input$sunflower_ybin,10)
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("sunflower_xbin", "Number of bins in X", 1, 50, 10, 1),
         sliderInput("sunflower_ybin", "Number of bins in Y", 1, 50, 10, 1),
         UIpointsymbol(),
         UIpointsize()
    )
  }
)