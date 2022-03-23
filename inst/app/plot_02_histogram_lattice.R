module[["histogram_lattice"]] <- list(
  label = "Histogram (lattice)",
  help  = "lattice::panel.histogram",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) 
  },
  code = function(analysis, group, data, input) {
    formula <- sprintf("~%s", input$analysis_var)
    if (length(input$group_var)>0) 
      formula <- paste0(formula, "|", paste0(input$group_var, collapse="*"))
    template("
             0:  library('lattice')
             1:  x <- cbind(numeric_data(data, select={{x}}),
             1:               character_data(data, select={{g}}))
             !1:  x <- numeric_data(data, select={{x}})
             0:  histogram({{formula}}, data=x, nint={{nint}}, type={{type}})
             ",
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             nint=getval(input$histogram_lattice_breaks, 10),
             type=txt(getval(input$histogram_lattice_type, "density")),
             formula=formula,
             nrow(group)>0 #1 
             )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("histogram_lattice_breaks", "Number of breaks", 2, 50, 10, 1),
        selectInput('histogram_lattice_type', "Type",
                    choices = c("Density"="density", "Percent"="percent", "Count"="count"))
    )
  }
)
