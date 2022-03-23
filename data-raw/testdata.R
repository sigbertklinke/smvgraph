swr <- function(x, size) { sample(x, size, TRUE) }

na <- function(v, n) {
  clv <- class(v)
  if (n<1) n <- ceiling(n*length(v))
  ind <- sample(length(v), n, replace=TRUE)
  if (clv[1]=="numeric") v[ind] <- NA_real_
  if (clv[1]=="logical") v[ind] <- NA
  if (clv[1]=="character") v[ind] <- NA_character_
  if (clv[1]=="integer") v[ind] <- NA_integer_
  if (clv[1]=="factor") v[ind] <- NA
  if (clv[1]=="ordered") v[ind] <- NA
  stopifnot(all(clv %in% class(v)))
  v
}

nan <- function(v, n) {
  clv <- class(v)
  if (n<1) n <- ceiling(n*length(v))
  ind <- swr(length(v), n)
  if (clv[1]=="numeric") v[ind] <- NaN
  stopifnot(all(clv %in% class(v)))
  v
}

inf <- function(v, n) {
  clv <- class(v)
  if (n<1) n <- ceiling(n*length(v))
  ind <- swr(length(v), n)
  if (clv[1]=="numeric") v[ind] <- Inf
  stopifnot(all(clv %in% class(v)))
  v
}

ninf <- function(v, n) {
  clv <- class(v)
  if (n<1) n <- ceiling(n*length(v))
  ind <- swr(length(v), n)
  if (clv[1]=="numeric") v[ind] <- -Inf
  stopifnot(all(clv %in% class(v)))
  v
}

value <- function(v, n, val) {
  clv <- class(v)
  if (n<1) n <- ceiling(n*length(v))
  ind <- swr(length(v), n)
  if (("factor" %in% clv) && !(val %in% levels(v))) levels(v) <- c(levels(v), val)
  v[ind] <- val
  stopifnot(all(clv %in% class(v)))
  v
}

testData <- function(...) {
   args  <- list(...)
   n     <- max(lengths(args))
   nargs <- names(args)
   stopifnot(length(nargs)>0)
   df <- list()
   for (i in seq_along(nargs)) {
     if (nargs[i]!='') df[[nargs[i]]] <- if (length(args[[i]])<n) swr(args[[i]], n) else args[[i]]
   }
   as.data.frame(df)
}


library("magrittr")
n  <- 26
testdata <- testData(xu=runif(n) %>% na(1) %>% nan(1) %>% inf(1) %>% ninf(1),
                     xn=rnorm(n, 0, 2) %>% na(1) %>% nan(1),
                     x0=0,
                     xi=as.integer(rnorm(n, 0, 2)) %>% na(1),
                     x2=0:1,
                     gf=as.factor(as.integer(rnorm(n, 0, 2))) %>% na(1),
                     go=ordered(as.integer(rnorm(n, 0, 2))) %>% value(1, 10),
                     gn=ordered(as.integer(rnorm(n, 0, 2))) %>% na(1),
                     gc=letters %>% na(1) %>% value(1, ''),
                     gl=swr(c(T,F), n) %>% na(1),
                     g0="constant",
                     g2=c(T,F)
                     )

sum(complete.cases(testdata))

usethis::use_data(testdata, overwrite = TRUE)
