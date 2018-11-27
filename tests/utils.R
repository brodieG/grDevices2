## Run all permutations of colorspace to colorspace translation
##
## @param time, if 0, just run the function, if greater than zero, run the
##   function that many times and time the result with microbenchmark.

color_to_color <- function(
  col, fun, spaces=names(col), strip.names=TRUE, time=0L
) {
  from.to <- subset(
    do.call(expand.grid, list(from=spaces, to=spaces, stringsAsFactors=FALSE)),
    from != to
  )
  fun_t <- function(...) {
    res <- try(
      if(time) {
        gc()
        mean(microbenchmark::microbenchmark(fun(...), times=time)[['time']])/1e9
      }
      else fun(...), silent=TRUE
    )
    if(inherits(res, 'try-error')) res <- 'error'
    if(strip.names) unname(res) else res
  }
  args <- c(list(fun_t, color=col[from.to$from]), from.to)
  res.raw <- do.call(Map, args)
  res <- matrix(
    if(time) numeric() else list(),
    nrow=length(spaces), ncol=length(spaces),
    dimnames=list(from=spaces, to=spaces)
  )
  if(time) res[as.matrix(from.to)] <- as.numeric(res.raw)
  else res[as.matrix(from.to)] <- res.raw
  res
}
## Given boundaries in a series of 3D spaces, interpolate points within them
##
## @param ranges a 2 x 3 x n array of the ranges, where n is the number of
##   colorspaces we want to interpolate in, the first dimension is the start and
##   end of the range, the second dimension represents the 3 dimensions of the
##   colorspace, and the last dimension the colorspaces.

interpolate_space <- function(
  ranges, steps=16, expand=c(0.2, 1e6), na=FALSE, nan=FALSE, inf=FALSE
) {
  stopifnot(
    identical(head(dim(ranges), 2), c(2L, 3L)), length(dim(ranges)) == 3
  )
  res <-lapply(seq_len(dim(ranges)[3]),
    function(x) {
      ranges <- split(ranges[,,x], col(ranges[,,x]))
      ranges.ex <- lapply(
        ranges,
        function(y) {
          c(
            seq(from=y[1], to=y[2], length.out=steps),
            min(y) - diff(range(y)) * expand,
            max(y) + diff(range(y)) * expand,
            if(na) NA, if(nan) NaN, if(inf) c(-Inf, Inf)
          )
        }
      )
      do.call(cbind, as.list(do.call(expand.grid, ranges.ex)))
    }
  )
  names(res) <- dimnames(ranges)[[3]]
  res
}
## Apply all.equal pairwise to corresponding elements of two matrices
##
## Helpful when trying to compare individual components of list matrices.

matrix.equal <- function(target, current) {
  stopifnot(
    !is.null(dim(target)), identical(dim(target), dim(current))
  )
  res <- matrix(
    logical(length(target)), nrow(target), nrow(current),
    dimnames=dimnames(target)
  )
  for(i in seq_len(nrow(target)))
    for(j in seq_len(ncol(target)))
      res[i, j] <- isTRUE(all.equal(target[[i, j]], current[[i, j]]))
  res
}
## Apply identical pairwise to corresponding elements of two matrices
##
## Helpful when trying to compare individual components of list matrices.

matrix.identical <- function(target, current) {
  stopifnot(
    !is.null(dim(target)), identical(dim(target), dim(current))
  )
  res <- matrix(
    logical(length(target)), nrow(target), nrow(current),
    dimnames=dimnames(target)
  )
  for(i in seq_len(nrow(target)))
    for(j in seq_len(ncol(target)))
      res[i, j] <- identical(target[[i, j]], current[[i, j]])
  res
}
