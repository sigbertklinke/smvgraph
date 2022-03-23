module[["time_series"]] <- list(
  label = "Time series",
  help  = "smvgraph::trend_season",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==1) && (analysis$class=="ts") 
  },
  code = function(analysis, group, data, input) {
    template("
0:     tts <- trend_season({{x}}, trend={{trend}}, season={{season}}) 
0:     plot(tts, type={{type}}, pch={{pch}}, cex={{cex}}, lwd={{lwd}}, lty={{lty}})
             ",
             x=sprintf("data$%s", row.names(analysis)),  
             trend=txt(getval(input$time_series_trend, "constant")),
             season=txt(getval(input$time_series_season, "none")),
             type=txt(getval(input$smvgraph_type, "b")),
             pch=getval(input$smvgraph_pch, 19),
             cex=getval(input$smvgraph_cex, 1),
             lwd=getval(input$smvgraph_lwd, 1),
             lty=getval(input$smvgraph_lty, 1)
             )
  },
  ui = function(analysis, group, data, input) {
    list(radioGroupButtons("time_series_trend", "Trend",
                           choices=c("Constant"="constant", "Linear"="linear", "Exponential"="exponential")),
         if (frequency(data[[rownames(analysis)]])>1) radioGroupButtons("time_series_season", "Seasonality",
                                                  choices=c("None"="none", "Additive"="additive", "Multiplicative"="multiplicative")),
         UIplottype(),
         UIpointsymbol(),
         UIpointsize(),
         UIlinetype(),
         UIlinewidth()
         )
  }
)
