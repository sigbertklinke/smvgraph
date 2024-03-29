module[["radarchart_fmsb"]] <- list(
  label = "Radar chart (fmsb)",
  help  = "fmsb::radarchart",
  packages = "fmsb",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && isTRUE(all(analysis$unique>1)) && (valid(data[,row.names(analysis)], 1, TRUE)>2)
  },
  code = function(analysis, group, data, input) {
    maxobs <- valid(data[,row.names(analysis)], 1, TRUE)
    template("
             0:   library('fmsb')
             0:   x     <- numeric_data(data, select={{x}})
             0:   xt    <- dimnames(x)[[1]]
             0:   keep  <- valid(x, 1)
             0:   x     <- x[keep,]
             1:   col   <- color_data(data, select={{g}})[keep]
             0:   opar  <- par(mar=c(0,0,1,0))
             0:   layout(toLayout({{len}}, sel={{sel}}))
             0:   xia   <- apply(x, 2, function(e) { c(max(e), min(e))})
             !1:  pfcol <- if ({{pfcol}}) 'grey' else NA 
             0:   #with_progress(message = 'Making plot', value = 0, { # progressbar in shiny 
             0:     for (i in {{from}}:{{to}}) {
             0:       xr <- rbind(xia, x[i,]) 
             !1:      radarchart(xr, pcol='black', title=xt[i], plwd=2, pfcol=pfcol, vlabels=if(i=={{from}}) NULL else '')
             1:       pfcol <- if ({{pfcol}}) col[i] else NA 
             1:       colo  <- if ({{pfcol}}) 'black' else col[i] 
             1:       radarchart(xr, pcol=colo, pfcol=pfcol, title=xt[i], plwd=2, vlabels=if(i=={{from}}) NULL else '')
             0:       #inc_progress(1/{{n}}, detail = i)
             0:     }
             0:   #})
             0:   par(opar)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             from=getval(input$smvgraph_obs[1], 1),
             to=getval(input$smvgraph_obs[2], min(25, maxobs)),
             len=getval(diff(input$smvgraph_obs)+1, 1),
             n=getval(diff(input$smvgraph_obs), 24)+1,
             sel=getval(input$radarchart_fmsb_sel, 0),
             pfcol=getval(input$radarchart_fmsb_pfcol, FALSE),
             nrow(group)>0     #1
            )
  },
  ui = function(analysis, group, data, input) {
    list(UIobservations(valid(data[,row.names(analysis)],1,TRUE)),
         sliderInput("radarchart_fmsb_sel", "Less rows <-> More rows", -5, 5, 0),
         checkboxInput('radarchart_fmsb_pfcol', "Fill polygons")
    )
  }
)
