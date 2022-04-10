#' availablePlots 
#'
#' Returns a data frame with columns about the available plots in `smvgraph`:
#' * `module`: the internal name used. If you want to call the Shiny app then you might need this.
#' * `label`: the label used in the Shiny app 
#' * `help`: the R help topic for the plot
#' * `packages`: packages which are required to make the plot
#' * `code`: if code block exists, should always be `TRUE`
#' * `ui`: if plot specific interactive UI elements exists
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
  retc <- rep(NA_character_, length(module))
  retl <- rep(NA, length(module))  
  ret <- data.frame(module=names(module), label=retc, help=retc, packages=retc, 
                    code=retl, ui=retl, condition=retc)
  for (i in seq_along(module)) {
    ret$condition[i] <- paste0(as.character(body(module[[ret$module[i]]]$usable))[-1], collapse=" ")
    ret$label[i]     <- toString(module[[ret$module[i]]]$label)
    ret$help[i]      <- toString(module[[ret$module[i]]]$help)
    ret$packages[i]  <- toString(module[[ret$module[i]]]$packages)
    ret$code[i]      <- !is.null(module[[ret$module[i]]]$code)
    ret$ui[i]        <- !is.null(module[[ret$module[i]]]$ui)
  }
  ret
}

installPlots <- function() {
  
}