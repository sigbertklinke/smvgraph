#' splot
suppressPackageStartupMessages({
  library("tools")
  library("devtools")
  library("formatR")
  library("highlight")
  library("shiny")
  library("shinydashboard")
  library("shinydashboardPlus")
  library("shinyWidgets")
  library("DT")
  library("sortable")
  library("base64enc")
  library("smvgraph")
})
set_logfile()
#msg <- paste0("Package: smvgraph ", utils::packageVersion("smvgraph"), ", (C) 2022- Sigbert Klinke, HU Berlin")
#loggit("DEBUG", msg)
#
oldpar <- graphics::par(no.readonly = TRUE)
on.exit(resetpar(oldpar))
#
args <- getShinyOption("smvgraph.param")
shinyOptions(smvgraph.param=NULL)
data <- if (is.null(args$file)) testdata else readRDS(args$file)
if (is.table(data)) data <- toDataframe(data)
if (is.ts(data)) data <- data.frame(xt=data)
if (is.null(args$plotmodule)) args$plotmodule <- ''
#if (is.null(args$analysis)) args$analysis <- names(data)[sapply(data, function(e){'numeric' %in% class(e)} )]
analysis_vars <- intersect(names(data), args$analysis)
unused_vars   <- setdiff(names(data), analysis_vars)
group_vars    <- NULL
# browser()
dvar        <- getVariableInfo(data)
#infotxt <- c(sprintf("Package: smvgraph %s", utils::packageVersion("smvgraph")), "(C) 2022- Sigbert Klinke, HU Berlin", "")
plotmodule  <- getModules('plot_*.R', path='.')
#infotxt   <- c(infotxt, attr(plotmodule, 'infotxt'))
loggit("DEBUG", paste0(attr(plotmodule, 'infotxt'), collapse="\n"))
#colormodule <- getModules('color_*.R', path='.')
#infotxt   <- c(infotxt, attr(colormodule, 'infotxt'))
#
shinyApp(#options=list(launch.browser=TRUE), 
         ui = shinydashboardPlus::dashboardPage(
           header = shinydashboardPlus::dashboardHeader(title="MM*Stat"
                                                        #                                                 leftUi = tagList(
                                                        #                                                   actionButton("smvgraph_action_plot", "Plot"),
                                                        #                                                   actionButton("smvgraph_action_rcode", "R code"),
                                                        #                                                   actionButton("smvgraph_action_help", "Help"),
                                                        #                                                   actionButton("smvgraph_action_variables", "Variables"),
                                                        #                                                   actionButton("smvgraph_action_info", "Info"),
                                                        #                                                   dropdownBlock(
                                                        #                                                     id = "point",
                                                        #                                                     title = "Point",
                                                        #                                                     badgeStatus = NULL,
                                                        #                                                     uiOutput("pointUI")
                                                        #                                                   ),
                                                        #                                                   dropdownBlock(
                                                        #                                                     id = "line",
                                                        #                                                     title = "Line",
                                                        #                                                     badgeStatus = NULL,
                                                        #                                                     uiOutput("lineUI")
                                                        #                                                   ),
                                                        #                                                   dropdownBlock(
                                                        #                                                     id = "text",
                                                        #                                                     title = "Text",
                                                        #                                                     badgeStatus = NULL,
                                                        #                                                     uiOutput("textUI")
                                                        #                                                   ),
                                                        #                                                   dropdownBlock(
                                                        #                                                     id = "legend",
                                                        #                                                     title = "Legend",
                                                        #                                                     badgeStatus = NULL,
                                                        #                                                     uiOutput("legendUI")
                                                        #                                                   ),
                                                        #                                                   dropdownBlock(
                                                        #                                                     id = "colour",
                                                        #                                                     title = "Color",
                                                        #                                                     badgeStatus = NULL,
                                                        #                                                     uiOutput("colorUI")
                                                        #                                                  )),
           ),
           sidebar = shinydashboardPlus::dashboardSidebar(
             tags$style(HTML("
                      .rank-list-item  { color: #000000; padding: 0px 5px !important; }
                      .rank-list-title { font-weight: bold; }
                      ")),
             uiOutput("ranklist")
           ),
           controlbar = shinydashboardPlus::dashboardControlbar(collapsed = FALSE, overlay=TRUE,
                                                                controlbarMenu(id="menu",
                                                                               controlbarItem(id="menu1", title="Plot(s)",
                                                                                              uiOutput("plotmodules")))
           ),
           body = shinydashboard::dashboardBody(
             tags$head(
               tags$style(
                 HTML(".shiny-notification { height: 100px; width: 800px; position:fixed; top: calc(50% - 50px); left: calc(50% - 400px); }
                  .shiny-plot-output  { height: calc(100vh - 200px) !important;}
                  .number             { color: rgb(21,20,181) ; }
                  .functioncall       { color: red ; }
                  .string             { color: rgb(169,169,169) ; } 
                  .keyword            { color: black; }
                  .argument           { color: rgb( 177,63,5) ; }
                  .comment            { color: rgb( 0,100,0) ; }
                  .roxygencomment     { color: rgb(0,151,255); }
                  .formalargs         { color: rgb(18,182,18); }
                  .eqformalargs       { color: rgb(18,182,18); }
                  .assignement        { color: rgb(55,55,98); }
                  .package            { color: rgb(150,182,37); }
                  .slot               { font-style:italic; }
                  .symbol             { color: b lack ; }
                  .prompt             { color: black ; }
                  .line               { color: gray ; }
                 "))
             ),
             #uiOutput("panel")
             tabsetPanel(
               id='smvgraph_tabset',
               tabPanel(title="Plot", value="smvgraph_panel_plot",
                        box(width=9, uiOutput("rplot")),
                        box(width=3, uiOutput("plotparam"), title="Plot options")),
               tabPanel(title="R code", value="smvgraph_panel_rcode", htmlOutput("rcode")),
               tabPanel(title="R Help", value="smvgraph_panel_rhelp", htmlOutput("rhelp")),
               tabPanel(title="Variables", value="smvgraph_panel_variables", DT::dataTableOutput("variables")),
#               tabPanel(title="Info", value="smvgraph_panel_info", verbatimTextOutput("info")),
               tabPanel(title="Log", value="smvgraph_panel_log", htmlOutput("log"))
             )
           )
         ),
         server = function(input, output, session) {
           htmlfile <- tempfile()
           onSessionEnded(function(){ unlink(htmlfile) })
           
#           rv <- reactiveValues(analysis_vars, group_vars, unused_vars)
           
           #    observeEvent(input$smvgraph_action_plot,      { updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_plot') } )
           #    observeEvent(input$smvgraph_action_rcode,     { updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_rcode') } )
           #    observeEvent(input$smvgraph_action_help,      { updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_rhelp') } )
           #    observeEvent(input$smvgraph_action_variables, { updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_variables') } )
           #    observeEvent(input$smvgraph_action_info,      { updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_info') } )
           observeEvent(input$smvgraph_plotmodule,       { if (input$smvgraph_tabset!='smvgraph_panel_plot') updateTabItems(session, 'smvgraph_tabset', 'smvgraph_panel_plot') } )
#           observeevent(input$SidebarCollapsed, {
#             if(input$SidebarCollapsed) {
#               
#             } else {
#               rv$analysis_vars <- input$analysis_vars
#               rv$group_vars <- input$analysis_vars
#             }
#           })
           
           
           # common UI elements
           output$pointUI <- renderUI({
             list(sliderInput("smvgraph_pch", "Point symbol", 0, 25, 19, 1),
                  sliderInput("smvgraph_cex", "Point size", 0, 3, 1, 0.05))
           })
           outputOptions(output, "pointUI", suspendWhenHidden = FALSE)
           
           output$lineUI <- renderUI({
             list(sliderInput("smvgraph_lty", "Line type", 0, 6, 1, 1),
                  sliderInput("smvgraph_lwd", "Line width", 0, 3, 1, 0.05))
           })
           outputOptions(output, "lineUI", suspendWhenHidden = FALSE)
           
           output$textUI <- renderUI({
             sliderInput("smvgraph_tex", "Text size", 0, 1.5, 1, 0.05)
           })
           outputOptions(output, "textUI", suspendWhenHidden = FALSE)
           
           output$legendUI <- renderUI({
             list(selectInput("smvgraph_legend", "Legend position",
                              choices=list("Top left"     = "topleft", 
                                           "Top right"    = "topright", 
                                           "Bottom left"  = "bottomleft",
                                           "Bottom right" = "bottomright",
                                           "Bottom"       = "bottom",
                                           "Left"         = "left",
                                           "Top"          = "top",
                                           "Right"        = "right",
                                           "Center"       = "center")),
                  sliderInput("smvgraph_lex", "Legend size", 0, 1.5, 1, 0.05))
           })
           outputOptions(output, "legendUI", suspendWhenHidden = FALSE)
           
           #    output$colorUI <- renderUI({
           #      if (length(colormodule)==0) return(NULL)
           #      browser()
           #      choices <- rep(NA_character_, length(colormodule))
           #      for (i in seq_along(colormodule)) choices[i] <- colormodule[[i]]$label
           #      ui <- list(selectInput("smvgraph_col", "Method",
           #                             choices=structure(as.list(1:length(colormodule)), names=choices)))
           #      for (i in seq_along(colormodule)) {
           #        if (!is.null(colormodule[[i]]$ui)) {
           #          ui[[length(ui)+1]] <- conditionalPanel(sprintf("input.smvgraph_col == %i", i),
           #                                                 colormodule[[i]]$ui(dvar[input$analysis_var,], dvar[input$group_var,], data, input)
           #          )
           #        }
           #      }
           #      ui
           #    })
           #    
           #    outputOptions(output, "colorUI", suspendWhenHidden = FALSE)
           
           # left side bar
           output$ranklist <- renderUI({
             if (!input$sidebarCollapsed) {
               bucket_list(
                 header=NULL,
                 add_rank_list(
                   text = "Analysis variable(s)",
                   labels = analysis_vars,
                   input_id = "analysis_var"
                 ),
                 add_rank_list(
                   text = "Grouping variable(s)",
                   labels = group_vars,
                   input_id = "group_var"
                 )   ,
                 add_rank_list(
                   text = "Unused variable(s)",
                   labels = unused_vars,
                   input_id = "unused_var"
                 )   
               )
             }
           })
           
           # right sidebar
           output$plotmodules <- renderUI({
             choices <- c()
             ids     <- c()
             for (i in 1:length(plotmodule)) {
               if (plotmodule[[i]]$usable(dvar[input$analysis_var,], dvar[input$group_var,], data, input)) {
                 choices <- c(choices, plotmodule[[i]]$label)
                 ids     <- c(ids, names(plotmodule)[i])
               }
             }

             if (length(choices)) {
               pty <- isolate(input$smvgraph_plotmodule)
               if (is.null(pty)) pty <- args$plotmodule
               sel <- match(pty, ids)
               sel <- if ((length(sel)==0) || is.na(sel)) NULL else ids[sel]
               return(radioButtons("smvgraph_plotmodule", "Plot(s)", 
                                   choices  = structure(as.list(ids), names=choices),
                                   selected = sel)
               )
             }
           })
           
           # panels
           output$variables <- DT::renderDataTable(dvar)
           
           output$rcode <- renderUI({
             if (is.null(input$smvgraph_plotmodule) || 
                 !plotmodule[[input$smvgraph_plotmodule]]$usable(dvar[input$analysis_var,], dvar[input$group_var,], data, input) ||
                 is.null(plotmodule[[input$smvgraph_plotmodule]]$code)) return("")
             code <- plotmodule[[input$smvgraph_plotmodule]]$code(dvar[input$analysis_var,], dvar[input$group_var,], data, input)
             res  <- try(formatCommands(code), silent=TRUE)   
             if ('try-error' %in% class(res)) {
               code <- c('<b style="color:red;">', code, '</b><br><b>', res, '</b>')
             } else {
               code <- c('<b>', res, '</b>')
             }
             HTML(code)
           })
           
           output$rhelp <- renderUI({
             if (is.null(input$smvgraph_plotmodule) || is.null(plotmodule[[input$smvgraph_plotmodule]]$help)) return("")
             help <- unlist(strsplit(plotmodule[[input$smvgraph_plotmodule]]$help, '::', fixed=TRUE))
             Rd2HTML(Rd_db(help[1])[[paste0(help[2], ".Rd")]], htmlfile, no_links = TRUE, package = help[1])
             includeHTML(htmlfile)
           })
           
           output$plot <- renderPlot({
             if (is.null(input$smvgraph_plotmodule) || 
                 !plotmodule[[input$smvgraph_plotmodule]]$usable(dvar[input$analysis_var,], dvar[input$group_var,], data, input) ||
                 is.null(plotmodule[[input$smvgraph_plotmodule]]$code)) return("")
             code <- plotmodule[[input$smvgraph_plotmodule]]$code(dvar[input$analysis_var,], dvar[input$group_var,], data, input)
             isolate(shinyOptions(smvgraph.current=reactiveValuesToList(input)))
             if ("plot.matrix" %in% .packages()) devtools::unload('plot.matrix') 
             loggit("DEBUG", paste0(code, collapse="\n"))
             eval(parse(text=code))
           })
           
           output$rplot <- renderUI({
             code <- ''
             if (!is.null(input$smvgraph_plotmodule) && !is.null(plotmodule[[input$smvgraph_plotmodule]]$code) && length(input$analysis_var)) {
               plotOutput("plot")
             } else {
#               logo <- base64enc::base64encode(system.file("app", "www", "wordcloud.png", package="smvgraph"))
               HTML(readLines(system.file("app", "www", "toc.html", package="smvgraph")))
             }
           })
           
           output$plotparam <- renderUI({
             if (is.null(input$smvgraph_plotmodule) || 
                 !plotmodule[[input$smvgraph_plotmodule]]$usable(dvar[input$analysis_var,], dvar[input$group_var,], data, input) ||
                 is.null(plotmodule[[input$smvgraph_plotmodule]]$ui)) return("")
             plotmodule[[input$smvgraph_plotmodule]]$ui(dvar[input$analysis_var,], dvar[input$group_var,], data, input)
           })
           
           output$info <- renderText({
             paste0(infotxt, collapse="\n")
           })
           
           output$log <- renderUI({
             html <- ''
             if (input$smvgraph_tabset=='smvgraph_panel_log') {
               logdf <- read_logs()
               oo <- options(digits.secs = 4)
               logdf$log_time <- strptime(logdf$log_time, "%Y-%m-%d %H:%M:%OS")   
               options(oo)
#               browser()
               html  <- paste0('<table><caption><h3>Package: smvgraph ', utils::packageVersion("smvgraph"), ' Logs</caption><tr><th>Time</th><th>Message</th></tr>')
               for (i in nrow(logdf):1) {
                 col <- 'white'
                 if (logdf$log_lvl[i]=="ERROR") col <- '#FFA07A' # LightSalmon
                 if (logdf$log_lvl[i]=="WARN") col <- '#E0FFFF'  # LightCyan
                 if (logdf$log_lvl[i]=="INFO") col <- '#ADD8E6'  # LightBlue
                 html <- c(html, paste0('<tr bgcolor="', col, '", valign="top"><td><pre>', logdf$log_time[i], '</pre></td><td><pre>', logdf$log_msg[i], '</pre></td></tr>' ))
               }
               html <- c(html, '</table>')
             }
             HTML(paste0(html, collapse="\n"))
           })
         }
)

