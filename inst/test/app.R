library("shiny")
browser()
files <- Sys.glob("module*.R")

for (file in files) source(file)

ui <- fluidPage(
  counterButton("counter1", "Counter #1"),
  actionButton("button1", "button")
)

server <- function(input, output, session) {
  counterServer("counter1")
}

shinyApp(ui, server)