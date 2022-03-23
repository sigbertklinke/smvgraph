
color_data.default <- function(x, select=NULL, colpal=grDevices::hcl.colors, ...) {
  vx     <- unlist(x) # convert to single vector
  args   <- list(...)
  out    <- args$out
  args$x      <- vx
  args$select <- select
  args$out    <- 'vector'
  vx    <- do.call("group_data", args)
  xu    <- unique(sort(vx))
  col   <- colgroup_datapal(length(xu))
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
