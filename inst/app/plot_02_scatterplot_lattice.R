module[["scatterplot_lattice"]] <- list(
  label = "Scatter plot (lattice)",
  help  = "lattice::B_00_xyplot",
  packages = "lattice",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==2) && (nrow(group>0)) && (prod(group$unique)<43)
  },
  packages = c("lattice"),
  code = function(analysis, group, data, input) {
    template("
             0:   library('lattice')
             !1:  grp <- character_data(data, select={{g}})
             1:   grp <- group_data(data, select={{g}}, out='data.frame')
             1:   names(grp) <- '.group'
             0:   x <- cbind(numeric_data(data, select={{x}}), grp)
             !1:   xyplot({{xy}}|{{group}}, data=x, pch={{pch}}, cex={{cex}})
             1:   xyplot({{xy}}|.group, group=grp$.group, data=x, pch={{pch}}, cex={{cex}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             group= paste0(row.names(group),collapse="+"),
             xy=paste0(row.names(analysis), collapse="~"),
             pch=getval(input$smvgraph_pch,1),
             cex=getval(input$smvgraph_cex,1),
             getval(input$smvgraph_single, FALSE)  #1  TRUE=just one group
            )
  },
  ui = function(analysis, group, data, input) {
    list(UIpointsymbol(),
         UIpointsize(),
         UImergegroups()
    )
  }
)
