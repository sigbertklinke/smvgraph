module[["density_sm"]] <- list(
  label = "Kernel density (sm)",
  help  = "sm::sm.density",
  packages = "sm",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (prod(group$unique)<43)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('sm')
             0:   x    <- numeric_data(data, select={{x}}, out='vector')
             0:   keep <- is.finite(x)
             0:   x    <- x[keep]
             !2:  h <- h.select(x)
             !2:  xlab <- sprintf('N=%.0f, h=%.3f', length(x), h*{{adjust}})
             !2:  sm.density(x, h=h*{{adjust}}, lwd=2, model={{model}}, xlab=xlab, ngrid=512)
             2:   col <- color_data(data, select={{g}})[keep]
             2:   ucol <- col[!duplicated(col)]
             2:   h    <- tapply(x, col, h.select)
             2:   h    <- exp(mean(log(h), na.rm=TRUE))*{{adjust}}
             2:   grp  <- factor(col, levels=ucol)
             2:   sm.density(x, h=h, group=grp, lwd=2, col=ucol, lty=rep('solid', length(ucol)), ngrid=512)
             2:   txt <- sapply(seq_along(ucol), function(i) { paste0('(', sum(col==ucol[i]), '/', signif(h,3), ')') })
             2:   legend({{pos}}, legend=paste(names(ucol), txt), fill=ucol, title=attr(col, 'title'), cex={{cex}}) 
             1:   rug(x)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             model=txt(if(getval(input$density_sm_normal, FALSE)) "Normal" else "none"),
             pos=txt(getval(input$smvgraph_legend, "topleft")),
             adjust=getval(input$density_sm_adjust, 1),
             cex=getval(input$smvgraph_lex, 1),
             getval(input$density_sm_rug, FALSE),  #1
             nrow(group)>0                     #2
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("density_sm_rug", "Show observations", FALSE),
         sliderInput("density_sm_adjust", "Multiplication factor for bandwidth", 0.05, 2, 1, 0.05), 
         if (nrow(group)==0) checkboxInput("density_sm_normal", "Compare to normal", FALSE),
         if (nrow(group)>0) UIlegend() else NULL,
         if (nrow(group)>0) UIlegendsize() else NULL
    )
  }
)

module[["density_sm2"]] <- list(
  label = "Kernel density (sm)",
  help  = "sm::sm.density",
  packages = "sm",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('sm')
             0:   x    <- numeric_data(data, select={{x}}, out='matrix')
             0:   keep <- is.finite(rowSums(x))
             0:   x    <- x[keep,]      
             0:   h    <- h.select(x)*{{adjust}}
             0:   sm.density(x, h=h, display='persp', phi={{phi}}, theta={{theta}})
             0:   usr <- par('usr')
             0:   text(usr[1]+0.05*diff(usr[1:2]), mean(usr[3:4]), sprintf('N=%.0f, h=(%.3f, %.3f)', nrow(x), h[1], h[2]),  srt=90) 
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             phi=getval(input$density_sm2_phi, 15),
             theta=getval(input$density_sm2_theta, 0),
             adjust=getval(input$density_sm2_adjust, 1),
             getval(input$density_sm2_rug, FALSE)   #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("density_sm2_adjust", "Multiplication factor for bandwidth", 0.05, 2, 1, 0.05), 
         sliderInput("density_sm2_theta", "Azimuth (theta)", -180, 180, 0, 1),
         sliderInput("density_sm2_phi", "Colatitude (phi)", -90, 90, 15, 1)
    )
  }
)
