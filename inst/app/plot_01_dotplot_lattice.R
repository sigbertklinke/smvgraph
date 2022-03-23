module[["dotplot_lattice_lattice"]] <- list(
  label = "Dot plot (lattice)",
  help  = "lattice::panel.dotplot",
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
             0:  dotplot({{formula}}, data=x)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             formula=formula
            )
  }#,
#  ui = function(analysis, group, data, input) {
#    list(checkboxInput("dotplot_lattice_lattice_doout", "Show outlier(s)", TRUE),
#         checkboxInput("dotplot_lattice_lattice_varwidth", "Width adjusted"),
#         sliderInput("dotplot_lattice_lattice_notch", "Notch width", 0, 1, 0, 0.05)
#    )
#  }
)
