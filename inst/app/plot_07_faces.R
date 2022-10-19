module[["faces_aplpack"]] <- list(
  label = "Chernoff faces",
  help  = "aplpack::faces",
  packages = "aplpack",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2) && (nrow(group)==0)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('aplpack')
             0:   x   <- numeric_data(data, select={{x}})
             0:   x   <- x[valid(x, 1),]
             0:   xt  <- dimnames(x)[[1]]
             0:   lo  <- toLayout({{len}}, sel = {{sel}})
             0:   ind <- {{from}}:{{to}}
             0:   faces(x[ind,,drop=FALSE],  face.type = {{type}}, nrow.plot=nrow(lo), ncol.plot=ncol(lo))
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             from=getval(input$smvgraph_obs[1], 1),
             to=getval(input$smvgraph_obs[2], 25),
             len=getval(diff(input$smvgraph_obs)+1, 1),
             sel=getval(input$faces_aplpack_sel, 0),
             type=getval(input$faces_aplpack_type, 0),
             nrow(group)>0     #1
            )
  },
  ui = function(analysis, group, data, input) {
    list(UIobservations(valid(data[,row.names(analysis)], 1, TRUE)),
         sliderInput("faces_aplpack_sel", "Less rows <-> More rows", -5, 5, 0),
         selectInput("faces_aplpack_type", "Face type",
                     choices=list("Line faces"=0, "Painted faces"=1, "Santa Claus faces"=2))
    )
  }
)
