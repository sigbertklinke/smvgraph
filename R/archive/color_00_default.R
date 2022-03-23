module[["color_default"]] <- list(
  label = "Default",
  help  = "grDevices::colors",
  packages = "grDevices",
  code = function(analysis, group, data, input) {
    template("0: col <- rep('black', {{n}})", n=nrow(data))
  }
)

module[["color_grouping"]] <- list(
  label = "Grouping variables",
  help  = "smvgraph::color_data",
  code = function(analysis, group, data, input) {
    template("
             !1: col <- color_data(data, select={{g}})
             1:  col <- rep('black', {{n}})
             ",
             n = nrow(data),
             g = sprintf("c(%s)", paste0("'", input$group_var, "'", collapse=', ')),
             length(input$group_var)==0)
  }
)
