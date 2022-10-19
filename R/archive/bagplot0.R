
PointInTriangle <- function (xy, p1, p2, factor) {
  cross <- function(a,b) { a[,1]*b[,2]-a[,2]*b[,1] }
  A <- matrix(c(0,0), nrow=1)
  B <- factor*xy[p1,,drop=FALSE]
  C <- factor*xy[p2,,drop=FALSE]
  xd <- cross(A,B)+cross(B,C)+cross(C,A)
  w  <- matrix(0, nrow=nrow(xy), ncol=3) 
  if (abs(xd)>1e-12) {
    w[,1] <- cross(B,C)+cross(xy, B-C)
    w[,2] <- cross(C,A)+cross(xy, C-A)
    w[,3] <- cross(A,B)+cross(xy, A-B)
    w <- w/xd
  }
  apply(w, 1, function(r) { all(r>=0, r<=1)})
}


bagplot2 <- function(x, y=NULL, factor=3, ...) {
  xlabel <- if (!missing(x)) deparse1(substitute(x))
  ylabel <- if (!missing(y)) deparse1(substitute(y))
  xy <- xy.coords(x, y, xlabel, ylabel)
  xy <- cbind(xy$x, xy$y)
  plot(xy, pch=19, cex=0.5)
  #depth  <- depth.halfspace(xy$x, xy$y, exact=TRUE)
  depth  <- c(1, 2, 1, 1, 3, 4, 5, 1, 7, 6, 1, 4, 2, 3, 1, 2, 1)
  depth  <- as.numeric(as.factor(depth))
  td     <- cumsum(rev(table(depth)))
  # central point
  nd           <- as.numeric(names(td))
  median.index <- which(depth==nd[1])
  median       <- apply(xy[median.index,,drop=FALSE], 2, mean)
  points(median[1], median[2], pch=8, col="red")
  # bag
  dk           <- nd[sum(td<=n/2)]
  bago.index   <- bagi.index   <- which(depth>=dk)
  bago.hull    <- bagi.hull    <- bagi.index[chull(xy[bagi.index,])]
  if (dk>1) {
    bago.index <- which(depth>=dk-1)
    bago.hull  <- bago.index[chull(xy[bago.index,])]
  } 
  polygon(xy[bago.hull,], border="blue")
  polygon(xy[bagi.hull,], border="blue")
  bag.index <- intersect(bago.hull, bagi.hull)
  browser()
  
                  
}

factor <- 1
par(mfrow=c(1,2))
library("ddalpha")
x <- c(5, 2, 1, 6, 3, 5, 4, 7, 4, 4, 3, 4, 5, 4, 6, 3, 3)
y <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 1, 14, 17, 18, 19, 20, 22)
bagplot2(y, x)
library("aplpack")
res <- bagplot(y, x, factor=factor)

par(mfrow=c(1,2))
mrfDepth::bagplot(mrfDepth::compBagplot(cbind(x,y)))
library("aplpack")
res <- aplpack::bagplot(y, x, factor=factor)



library("ddalpha")
x <- c(5, 2, 1, 6, 3, 5, 4, 7, 4, 4, 3, 4, 5, 4, 6, 3, 3)

y <- c(2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 1, 14, 17, 18, 19, 20, 22)

xy <- cbind(y,x)
n  <- nrow(xy)
#depth  <- depth.halfspace(xy, xy, seed=Sys.time())
depth  <- c(1, 2, 1, 1, 3, 4, 5, 1, 7, 6, 1, 4, 2, 3, 1, 2, 1)
depth  <- table(as.factor(depth))
sdepth <- sort(unique(depth), decreasing=TRUE)
lbag   <- list()
i <- 1
while(i<=length(sdepth)) {
  bag       <- which(depth>=sdepth[i])
  bag.hull  <- bag[chull(xy[bag,1], xy[bag,2])]
  lbag[[i]] <- list(inside=bag, hull=bag.hull)
  if (length(lbag[[i]]$inside)>n/2) break
  i <- i+1
}

bagdepth <- quantile(depth, 0.5)
bag      <- which(depth>=bagdepth)
bag.hull <- bag[chull(xy[bag,1], xy[bag,2])]


bagxy    <- xy[bag.hull,]  
median <- which(depth==max(depth))
medianxy <- apply(xy[median,,drop=FALSE], 2, mean)

cxy <- scale(xy, center=medianxy, scale=FALSE)
fence <- rep(FALSE, nrow(xy))
for (i in 1:length(bag)) {
  fence <- fence | if (i==1) PointInTriangle(cxy, bag[length(bag)], bag[1], factor) else PointInTriangle(cxy, bag[i-1], bag[i], factor) 
}
fence      <- which(fence)
fence.hull <- fence[chull(xy[fence,1], xy[fence,2])]
fencexy <- xy[fence.hull,]

par(mfrow=c(1,2))
plot(xy, pch=19, cex=0.25)
polygon(bagxy)
points(medianxy[1], medianxy[2], col="red")
polygon(fencexy)

library("aplpack")
res <- bagplot(xy, factor=factor)

