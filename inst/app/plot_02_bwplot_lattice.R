module[["bwplot_lattice"]] <- list(
  label = "Box plot (lattice)",
  help  = "lattice::panel.bwplot",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)>0)
  },
  code = function(analysis, group, data, input) {
    formula <- sprintf("%s~%s", input$analysis_var[1], input$group_var[1])
    if (length(input$group_var)>1) 
      formula <- paste0(formula, "|", paste0(input$group_var[-1], collapse="+"))
    template("
             0:  library('lattice')
             0:  x   <- cbind(numeric_data(data, select={{x}}),
             0:               character_data(data, select={{g}}))
             0:  bwplot({{formula}}, data=x, varwidth={{width}}, notch={{notch}}, notch.frac={{nfrac}}, do.out={{doout}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             width=getval(input$bwplot_lattice_varwidth, FALSE),
             notch=getval(input$bwplot_lattice_notch>0, FALSE),
             nfrac=getval(input$bwplot_lattice_notch, 0),
             doout=getval(input$bwplot_lattice_doout, TRUE),
             formula=formula
            )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("bwplot_lattice_doout", "Show outlier(s)", TRUE),
         checkboxInput("bwplot_lattice_varwidth", "Width adjusted"),
         sliderInput("bwplot_lattice_notch", "Notch width", 0, 1, 0, 0.05)
    )
  }
)
