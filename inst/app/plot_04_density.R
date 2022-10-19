module[["density"]] <- list(
  label = "Kernel density",
  help  = "stats::density",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (prod(analysis$unique)>1) && (valid(data[,row.names(analysis)], 1, TRUE)>9) & (prod(group$unique)<43)
  },
  code = function(analysis, group, data, input) {
    template("
             0:  x    <- numeric_data(data, select={{x}}, out='vector')
             0:  keep <- valid(x, 1)
             0:  x    <- x[keep]
             !2: dens <- density(x, bw={{bw}}, kernel={{kernel}}, adjust={{adjust}})
             !2: plot(dens, lwd=2)
             2:  col  <- color_data(data, select={{g}})[keep]
             2:  ucol <- col[!duplicated(col)]
             2:  dens <- lapply(seq_along(ucol), function(i) { ind <- (col==ucol[i]); if (sum(ind)>1) density(x[ind], bw={{bw}}, kernel={{kernel}}, adjust={{adjust}}) else list(x = x[ind], y =0 , bw=NA, n=1)})
             2:  xlim <- range(x, na.rm=TRUE)+c(-3,3)*{{adjust}}*bw.{{bwfun}}(x)
             2:  ylim <- c(0, max(sapply(dens, function(e) { max(e$y) })))
             2:  plot(0, mean(x), xlim=xlim, ylim=ylim, type='n', ylab='Densities', xlab='x')
             2:  txt <- sapply(seq_along(ucol), function(i) { lines(dens[[i]], col=ucol[i], lwd=2); paste0('(', dens[[i]]$n, '/', signif(dens[[i]]$bw,3), ')') })
             2:  legend({{pos}}, legend=paste(names(ucol), txt), fill=ucol, title=attr(col, 'title'))  
             1:  rug(x)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             pos=txt(getval(input$smvgraph_legend, "topleft")),
             bw=txt(getval(input$density_bw, "nrd0")),
             bwfun=getval(input$density_bw, "nrd0"),
             kernel=txt(getval(input$density_kernel, "gaussian")),
             adjust=getval(input$density_adjust, 1),
             pos=txt(getval(input$smvgraph_legend, "topleft")),
             getval(input$density_rug, FALSE),  #1
             nrow(group)>0                      #2
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("density_rug", "Show observations", FALSE),
         sliderInput("density_adjust", "Multiplication factor for bandwidth", 0.05, 2, 1, 0.05), 
         selectInput("density_bw", "Basis bandwith", selected="SJ",
                       choices=list("Robust Silverman"="nrd0",
                                    "Scott variation"="nrd",
                                    "Unbiased crossvalidation"="ucv",
                                    "Biased crossvalidation"="bcv",
                                    "Sheater-Jones pilot"="SJ")),
         selectInput("density_kernel", "Kernel function",
                       choices=list("Gaussian"="gaussian",
                                    "Epanechnikov"="epanechnikov",
                                    "Rectangular"="rectangular",
                                    "Triangular"="triangular",
                                    "Biweight/Quartic"="biweight",
                                    "Cosine"="cosine",
                                    "Optcosine"="optcosine")),
         if (nrow(group)>0) UIlegend() else NULL
      )
  }
)