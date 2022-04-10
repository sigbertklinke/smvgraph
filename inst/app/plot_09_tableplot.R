module[["tableplot"]] <- list(
  label = "Table plot",
  help  = "tabplot::tableplot",
  packages = "mtennekes/tabplot",
  usable = function(analysis, group, data, input) {
     (nrow(analysis)>1) && (nrow(group)==0) 
  },
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    template("
    0: # devtools::install_github('mtennekes/tabplot')
    0: library('tabplot')
    0: tableplot({{dat}}, sortCol={{sortCol}}, decreasing={{decreasing}})
             ",
             dat=sprintf("data[,c(%s)]", paste0("'", rn, "'", collapse=", ")),
             sortCol=getval(input$tableplot_sortcol, 1),
             decreasing=getval(input$tableplot_decreasing, FALSE)
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("tableplot_sortcol", "Sorting column", 1, nrow(analysis), 1, 1),
         checkboxInput("tableplot_decreasing", "Decreasing", TRUE)
    )
  }
)

