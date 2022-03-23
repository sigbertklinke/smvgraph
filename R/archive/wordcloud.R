library("wordcloud2")
library("tools")
#
files    <- Sys.glob("plot_*.R")
files    <- files[order(file_path_sans_ext(files))]
plottype <- list()
for (file in files) {
  res <- try(eval(parse(file)), silent=TRUE)
}
#

d <- packageDescription("smvgraph")
allgraphics.word <- paste("smvgraph", d$Version)
allgraphics.freq <- 4

for (i in 1:length(plottype)) {
  allgraphics.word <- c(allgraphics.word, module[[i]]$label)
  allgraphics.freq <- c(allgraphics.freq, rep(2, length(module[[i]]$label)))
  allgraphics.word <- c(allgraphics.word, module[[i]]$packages)  
  allgraphics.freq <- c(allgraphics.freq, rep(1.5, length(module[[i]]$packages)))
} 
dups             <- duplicated(allgraphics.word)
allgraphics.word <- allgraphics.word[!dups]
allgraphics.freq <- allgraphics.freq[!dups]

library(webshot)
webshot::install_phantomjs()
# Make the graph
my_graph=wordcloud2(data.frame(word=allgraphics.word, freq=allgraphics.freq), size=0.25,
                    minRotation = -pi/2, maxRotation = pi/2, rotateRatio = 0.5)
# save it in html
library("htmlwidgets")
saveWidget(my_graph,"www/wordcloud.html",selfcontained = F)
# and in pdf
webshot("www/wordcloud.html","www/wordcloud.png", delay =5, vwidth = 640, vheight=480)

