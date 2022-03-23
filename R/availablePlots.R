#' availablePlots 
#'
#' Returns a data frame with three columns about all available plots in `smvgraph`:
#' * `plottype`: the internal name used. If you want to call the Shiny app then you might need this.
#' * `label`: the label used in the Shiny app 
#' * `condition`: the condition when a plot is offered in the Shiny app to the user
#' 
#' To understand `condition`:
#' 
#' * `nrow(analysis)`: the number of variables in "Analysis" field
#' * `nrow(group)`: the number of variables in "Grouping by" field
#' * `xxx$unique`: the number of unique values in a variable, for other elements then `unique` see the "Variable" panel of the Shiny app
#' 
#' ![Start screen of smvgraph app](smvgraph.png)
#' 
#' `13` was choosen because twelve has the largest number of divisors below 20 and `43` was choosen because forty-two is the answer of the 
#' \href{https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#The_Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_is_42}{ultimate question} ;)
#' 
#' @return a data frame with information about all available plots
#' @export
#'
#' @examples
#' availablePlots()
availablePlots <- function() {
  browser()
  module <- list()
  files <- Sys.glob(file.path(system.file("app", package="smvgraph"), "plot_*.R"))
  for (file in files) try(eval(parse(file)), silent=TRUE)
  retc <- rep(NA_character_,length(module))
  retl <- rep(NA,length(module))  
  ret <- data.frame(module=names(module), label=retc,
                    label.exists=retl, help.exists=retl, packages.exists=retl, usable.exists=retl,
                    code.exists=retl, ui.exists=retl,  condition=retc)
  for (i in seq_along(module)) {
    ret$label[i]           <- module[[i]]$label
    ret$condition[i]       <- paste0(as.character(body(module[[i]]$usable))[-1], collapse=" ")
    ret$label.exists[i]    <- !is.null(module[[i]]$label)
    ret$help.exists[i]     <- !is.null(module[[i]]$help)
    ret$packages.exists[i] <- !is.null(module[[i]]$packages)
    ret$usable.exists[i]   <- !is.null(module[[i]]$usable)
    ret$code.exists[i]     <- !is.null(module[[i]]$code)
    ret$ui.exists[i]       <- !is.null(module[[i]]$ui)
  }
  ret
}