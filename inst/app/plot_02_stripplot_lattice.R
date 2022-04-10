module[["stripplot_lattice"]] <- list(
  label = "Strip chart (lattice)",
  help  = "lattice::panel.stripplot_lattice",
  packages = c("lattice"),
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
             0:  stripplot({{formula}}, data=x, jitter.data={{jitter}}, amount={{amount}}, pch=19)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             jitter=getval(input$stripplot_lattice_amount>0, FALSE),
             amount=getval(input$stripplot_lattice_amount, 0),
             formula=formula
            )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("stripplot_lattice_amount", "Amount of jittering", 0, 0.5, 0, 0.05))
  }
)
