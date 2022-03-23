module[["barplot_numeric"]] <- list(
  label = "Needle chart",
  help  = "graphics::plot.default",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (nrow(group)==0) 
  },
  code = function(analysis, group, data, input) {
    template("
             0: x <- numeric_data({{x}})
             0: plot(table(x))
             ", 
             x=sprintf("data$%s", row.names(analysis)))
  }
)