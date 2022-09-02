module[["radarchart_mass"]] <- list(
  label = "Radar chart",
  help  = "graphics::stars",
  packages = "MASS",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)>2)
  },
  code = function(analysis, group, data, input) {
    template("
             0:   library('MASS')
             0:   x   <- numeric_data(data, select={{x}})
             0:   xn <- normalize(x)
             0:   xt  <- dimnames(x)[[1]]
             1:   col <- color_data(data, select={{g}})
             0:   lo  <- toLayout({{len}}, sel = {{sel}}, unused=2)
             0:   ind <- {{from}}:{{to}}
             0:   loc <- 2.5*attr(lo, 'mass')
             0:   key <- 2.5*c(ncol(lo)-0.5, 1)
             !1:   stars(xn[ind,,drop=FALSE], lwd=2, locations=loc, key.loc=key, scale=FALSE, draw.segments={{fn}})
             1:   stars(xn[ind,,drop=FALSE], lwd=2, locations=loc, key.loc=key, col.stars=col[ind], scale=FALSE)
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             from=getval(input$smvgraph_obs[1], 1),
             to=getval(input$smvgraph_obs[2], 25),
             len=getval(diff(input$smvgraph_obs)+1, 1),
             sel=getval(input$radarchart_mass_sel, 0),
             pfcol=getval(input$radarchart_mass_pfcol, FALSE),
             fn=getval(input$radarchart_mass_fn, FALSE),
             nrow(group)>0     #1
            )
  },
  ui = function(analysis, group, data, input) {
    list(UIobservations(nrow(data)),
         sliderInput("radarchart_mass_sel", "Less rows <-> More rows", -5, 5, 0),
         if (nrow(group)==0) checkboxInput("radarchart_mass_fn", "Polar area (F. Nightingale)") else NULL
    )
  }
)
