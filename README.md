# smvgraph

Various visualisations of univariate and multivariate graphs (e.g. mosaic diagram, scatterplot matrix, Andrews curves, parallel coordinate diagram, radar diagram and Chernoff plots) as well as clustering methods (e.g. k-means, agglomerative, EM clustering and DBSCAN) are implemented as a Shiny app. The app allows interactive changes, e.g. of the order of variables. It is intended for use in teaching.

```r
library("smvgraph")
#
splot(iris)
# 
smosaic(Titanic)
sandrews(iris)
schernoff(iris)
spairs(iris)
sparcoord(iris)
sradar(iris)
#
skmeans(iris)
shclust(iris)
smclust(iris)
sdbscan(iris)
```

# Installation  

## From CRAN

```R
install.packages("smvgraph")
```

## From github

Note that from github you install the current development version.

```R
library("devtools")
install_github("sigbertklinke/smvgraph")
```
