module[["barchart_lattice"]] <- list(
  label = "Bar chart (lattice)",
  help   = "lattice::panel.barchart",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && (nrow(group)==0) && isTRUE(all(analysis$unique<13))
  },
  packages = c("lattice"),
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    template("
             0: library('lattice')
             0: x   <- character_data(data, select={{x}})
             0: tab <- table(x)
             1: tab <- proportions(tab)
             0: barchart(tab, stack={{stack}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(analysis)), fun="c"),
             stack=getval(input$barchart_lattice_stack, TRUE),
             getval(input$barchart_lattice_relative, FALSE) #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(checkboxInput("barchart_lattice_stack", "Stacked"),
         checkboxInput("barchart_lattice_relative", "Relative")
    )
  }
)