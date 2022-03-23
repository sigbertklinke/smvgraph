#' color_data
#'
#' Assigns a color to the data `x` based on the color palette `colpal`.
#'
#' @param x vector, matrix, or data frame
#' @param select vector: indicating columns to select (default: \code{1}) 
#' @param colpal color palette (default: [grDevices::hcl.colors])
#' @param ... further parameters to [group_data]
#'
#' @return a color vector
#' @export
#'
#' @examples
#' color_data(iris)
#' color_data(as.matrix(iris)
#' color_data(iris$Species)
color_data     <- function(x, ...)     { UseMethod("color_data") }
group_data     <- function(x, ...)     { UseMethod("group_data") }
character_data <- function(x, ...) { UseMethod("character_data") }

convertTo <- function(x, coln, rown, title, out=c("data.frame", "matrix", "vector")) {
  browser()
  out <- match.arg(out)
  if (is.data.frame(x)) {
    if (out=='data.frame') {
      ret           <- x
      names(ret)    <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret          <- matrix(unlist(x), ncol=ncol(x))
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret        <- unlist(x)
      names(ret) <- rep(row.names(x), length.out=length(x))
    }
  } else if (is.matrix(x)) {
    if (out=='data.frame') {
      ret <- structure(as.data.frame(x), title=title)
      names(ret)    <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret <- structure(x, title=title)
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret <- x
      attributes(ret)    <- NULL
      attr(ret, "title") <- title
      names(ret)         <- rep(row.names(x), length.out=length(x))
    }
  } else { # vector
    if (out=='data.frame') {
      ret <- data.frame(x=x)
      names(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='matrix') {
      ret <- matrix(unlist(x), ncol=ncol(x))
      colnames(ret) <- coln
      rownames(ret) <- rown
    }
    if (out=='vector') {
      ret        <- unlist(x)
      names(ret) <- rep(row.names(x), length.out=length(x))
    }
  }
  attr(ret, 'title') <- if(is.null(title)) paste0(coln, paste=",") else title
  ret
}

color_data.default <- function(x, select=NULL, colpal=grDevices::hcl.colors, ...) {
  vx     <- unlist(x) # convert to single vector
  args   <- list(...)
  out    <- args$out
  args$x      <- vx
  args$select <- select
  args$out    <- 'vector'
  vx    <- do.call("group_data", args)
  xu    <- unique(sort(vx))
  col   <- colpal(length(xu))
  out   <- if(is.null(out)) 'vector' else out
  convertTo(x, coln=attr(x, "title"), rown=names(vx), title=attr(x, "title"), out=out)
}

color_data.matrix <- function(x, select=NULL, colpal=grDevices::hcl.colors, ...) {
  vx          <- x
  args        <- list(...)
  out         <- args$out
  args$x      <- vx
  args$select <- select
  args$out    <- 'vector'
  vx    <- do.call("group_data", args)
  xu    <- unique(sort(vx))
  col   <- colpal(length(xu))
  out   <- if(is.null(out)) 'vector' else out
  convertTo(x, coln=paste0(colnames(x), collapse=";"), rown=row.names(x), title=attr(x, "title"), out=out)
}

color_data.data.frame <- function(x, select=NULL, colpal=grDevices::hcl.colors, ...) {
  browser()
  vx          <- x
  args        <- list(...)
  out         <- args$out
  args$x      <- vx
  args$select <- select
  args$out    <- 'vector'
  vx    <- do.call("group_data", args)
  xu    <- unique(sort(vx))
  col   <- colpal(length(xu))
  out   <- if(is.null(out)) 'vector' else out
  convertTo(x, coln=attr(x, "title"), rown=names(vx), title=attr(x, "title"), out=out)
}

numeric_data   <- function(x, ...)   { UseMethod("numeric_data") }

numeric_data.default <- function(x, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass, colname=NULL) {
  out <- match.arg(out) 
  vx  <- x
  if (!is.numeric(vx)) {
    vx  <- factor(as.character(vx))
    vx <- structure(as.numeric(vx), levels=levels(vx))
  }
  coln <- if (is.null(colname)) attr(x, 'title') else colname
  na.action(convertTo(vx, coln=coln, rown=names(x), title=attr(x, 'title'), out=out))
}

numeric_data.matrix <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass) {
  browser()
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select]
  if (!is.numeric(vx)) {
    vx  <- apply(vx, 2, function(v) {
      as.numeric(factor(as.character(v)))
    })
  }
  na.action(convertTo(vx, coln=colnames(x), rown=rownames(x), title=attr(x, 'title'), out=out))
}

numeric_data.data.frame <- function(x, select=NULL, out=c("data.frame", "matrix", "vector"), na.action=stats::na.pass) {
  out <- match.arg(out) 
  vx  <- if (is.null(select)) x else x[,select]
  vx  <- lapply(vx, function(v) {
    if (is.numeric(v)) return(v)
    v <- factor(as.character(v))
    structure(as.numeric(v), levels=levels(v))
  })
  na.action(convertTo(as.data.frame(vx), coln=colnames(x), rown=rownames(x), title=attr(x, 'title'), out=out))
}

#color_data <- function(x, select=NULL, colpal=grDevices::hcl.colors, ...) {
#  browser()
#  args <- list(...)
#  args$x <- x
#  args$select <- select
#  if (is.null(args$out)) args$out <- 'matrix'
#  x <- do.call(group_data, args)
#  xu  <- unique(sort(x))
#  col <- colpal(length(xu))
#  convertTo(x, names(x), coln=attr(x, "title"), rown=names(x))
#  structure(col[match(x, xu)], names=names(x), title=)
#}