module[["vioplot_lattice"]] <- list(
  label = "Violin plot (lattice)",
  help  = "lattice::panel.violin",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)>0)
  },
  packages = c("lattice"),
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    gn <- row.names(group)
    formula <- sprintf("%s~%s", rn[1], gn[1])
    if (length(input$group_var)>1) 
      formula <- paste0(formula, "|", paste0(gn[-1], collapse="+"))
    template("
             0:  library('lattice')
             0:  x   <- cbind(numeric_data(data, select={{x}}),
             0:               character_data(data, select={{g}}))
             0:  bwplot({{formula}}, data=x, varwidth={{width}}, adjust={{adjust}}, panel=panel.violin)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             width=getval(input$vioplot_lattice_varwidth, FALSE),
             adjust=getval(input$vioplot_lattice_adjust, 1),
             formula=formula
            )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("vioplot_lattice_varwidth", "Width adjusted"),
         sliderInput("vioplot_lattice_adjust", "Multiplication factor for bandwidth", 0.05, 2, 1, 0.05)
    )
  }
)
