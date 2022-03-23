module[["color_hclust"]] <- list(
  label = "Hierarcical clustering",
  help  = "stats::hclust",
  code = function(analysis, group, data, input) {
    template("
             0: x   <- numeric_data(data, select={{x}})
             0: d   <- dist(x, {{dist}})
             0: hc  <- hclust(d, {{method}})
             0: col <- color_data(cuttree(hc, {{no}}))",
             x      = as_param(txt(row.names(analysis)), fun="c"),
             dist   = txt(getval(input$smvgraph_distance), "euclidean"),
             method = txt(getval(input$color_hlcust_method), "euclidean"), 
             no     = getval(input$color_hclust_no, 2))
  },
  ui = function(analysis, group, data, input) {
    list(
      sliderInput("color_hclust_no", "Number of clusters",
                  2, min(15, nrow(data)), 2, 1),
      UIdatanormalization(),
      selectInput("color_hclust_method", "Clustering Method",
                  c("WardD"= "ward.D", c("Ward.D2"="ward.D2", "Single linkage"="single", "Complete Linkage"="complete", 
                    "Average linkage/UPGMA"="average", "McQuitty/WPMGA"= "mcquitty", "Median linkage/WPGMC"="median",
                    "Centroid linkage/UPGMC"="centroid"),
                    selected="single"))
    )
  }
)
