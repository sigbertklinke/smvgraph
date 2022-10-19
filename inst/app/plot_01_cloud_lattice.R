module[["cloud_lattice"]] <- list(
  label = "Scatter plot 3D (lattice)",
  help   = "lattice::panel.cloud",
  usable = function(analysis, group, data, input) {
    (nrow(analysis)==3) && isTRUE(all(analysis$unique>1)) && isTRUE(all(group$unique<13)) 
  },
  packages = c("lattice"),
  code = function(analysis, group, data, input) {
    rn <- row.names(analysis)
    gn <- row.names(group)
    formula <- sprintf("%s~%s+%s", rn[1], rn[2], rn[3])
    if (nrow(group)>0) formula <- paste0(formula, "|", paste0(gn, collapse="*"))
    template("
             0:  library('lattice')
             !1: x   <- numeric_data(data, select={{x}}) 
             1:  x   <- cbind(numeric_data(data, select={{x}}),
             1:               character_data(data, select={{g}}))
             0:  cloud({{formula}}, data=x, screen=list(z={{rotz}}, x={{rotx}}), distance={{dist}})
             ", 
             x=as_param(txt(row.names(analysis)), fun="c"),
             g=as_param(txt(row.names(group)), fun="c"),
             dist=getval(input$cloud_lattice_dist, 0.2),
             rotz=getval(input$cloud_lattice_rotz, 40),
             rotx=getval(input$cloud_lattice_rotx, -60),
             formula=formula,
             nrow(group)>0  #1
    )
  },
  ui = function(analysis, group, data, input) {
    list(sliderInput("cloud_lattice_dist", "Distance", 0, 1, 0.2, 0.05),
         sliderInput("cloud_lattice_rotz",  "Rotation z-axis", -180, 180, 40, 2),
         sliderInput("cloud_lattice_rotx",  "Rotation x-axis", -90, 90, -60, 2)
    )
  }
)