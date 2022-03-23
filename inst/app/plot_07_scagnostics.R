module[["scagnostics"]] <- list(
  label = "Scagnostics",
  help  = "scagnostics::scagnostics",
  packages = c("rJava", "scagnostics", "devtools", "plot.matrix"),
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && isTRUE(all(analysis$unique>1)) && (nrow(group)==0) 
  },
  code = function(analysis, group, data, input) {
    template("
             0:  library('scagnostics')
             0:  library('plot.matrix')
             0:  x <- numeric_data(data, select={{x}})
             0:  x <- x[is.finite(rowSums(x)),]
             0:  names(x) <- abbreviate(names(x))
             0:  sc <- structure(scagnostics(x), class='matrix')
             0:  main <- 'Scagnostics coefficients'
             1:  sc   <- t(apply(sc, 1, function(v) { scale(v) }))
             1:  main <- paste(main, '(scaled)')
             2:  sc   <- t(apply(sc, 1, function(v) { (v-min(v, na.rm=TRUE))/diff(range(v, na.rm=TRUE)) }))
             2:  main <- paste(main, '(minmax)')
             0:  op   <- par(mar=c(6.1, 6.1, 4.1, 4.1))
             0:  plot(sc, las=2, xlab='', ylab='', main=main, digits={{digits}}, cex={{cex}})
             0:  devtools::unload('plot.matrix')
             0:  par(op)
             ", 
             x=sprintf("c(%s)", paste0("'", row.names(analysis), "'", collapse=", ")),
             digits=1+getval(input$scagnostics_digits, FALSE),
             cex=getval(input$smvgraph_cexfont, 1),
             getval(input$scagnostics_scale, "none")=="scale",      #1
             getval(input$scagnostics_scale, "none")=="minmax"      #2
            )
  },
  ui = function(analysis, group, data, input) {
    list(selectInput("scagnostics_scale", "Scale coefficients",
                     choices=list("None"="none", "MinMax"="minmax", "Scale"="scale")),
         checkboxInput("scagnostics_digits", "Two digits"),
         UItextsize()
         )
  }
)
