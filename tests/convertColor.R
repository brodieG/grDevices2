## For testing simplicity we made a copy of `grDevices` that contains just the
## color conversion functions and called it `grDevices0`.  If you have a recent
## version of r-devel (>= r75340) you can just use `grDevices` directly.

library(grDevices0)
## library(grDevices)   ## if you have r-devel > r75340

## The minimal changes 'level-1' patch is `grDevices1`, and the 'level-2' patch
## is `grDevices2`.  To replicate these tests you can checkout the 'level-1'
## branch and the 'level-2' branch to different directories and install each of
## those as you would a source package.  See github readme for instructions:
##
## https://github.com/brodieG/grDevices2

# - Sample Input Spaces --------------------------------------------------------

## Generate 8000 colors sampled evenly within (and outside) the typical ranges
## of each color space.  We use helper functions from `tests/utils.R` to
## generate samplings of the color spaces and applying the conversion functions
## to all permutations of input/output color spaces.
##
## utils.R defines:
## * color_to_color: apply a function to every permutation of from/to color
##   spaces contained in the color space list input.  Returns a list matrix with
##   the result of the color conversion.  Requires `microbenchmark` for timing
##   version.  Column names are dropped.
## * interpolate_space: given an array containing the bounds of a color space,
##   interpolate (8000 by default) colors within those bounds and also some
##   outside those bounds.
## * matrix.identical/equal: run `identical` or `all.equal` element-wise on
##   list-matrices.

if(!require(microbenchmark)) warning("Timed tests require microbench ark")
source('tests/utils.R')

ranges.raw <- c(
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0, 100,-128, 128,-128, 128,  # Lab
  0, 100,-180, 180,-180, 180   # Luv
)
spaces <- c('Apple RGB', 'sRGB', 'CIE RGB', 'XYZ', 'Lab', 'Luv')

## ranges is an array containing the min and max values for each dimension of
## each supported color space

ranges <- array(
  ranges.raw, dim=c(2, 3, length(ranges.raw) / (2 * 3)),
  dimnames=list(range=c('lo', 'hi'), NULL, space=spaces)
)
ranges

# For each input space, generate permutation of values in range and outside of
# In this case we're generating 8000 permutations (16 + 4) ^ 3, where we sample
# 16 values within range, and 4 outside.

space.input <- interpolate_space(ranges, steps=16)
str(space.input)

# - Compare Convert Color ------------------------------------------------------

## All colors

cc0.0 <- color_to_color(space.input, fun=convertColor)
cc1.0 <- color_to_color(space.input, fun=grDevices1::convertColor)
cc2.0 <- color_to_color(space.input, fun=grDevices2::convertColor)

all.equal(cc0.0, cc1.0)   # TRUE
all.equal(cc0.0, cc2.0)   # TRUE

## Single color, pick midpoint of ranges

space.one <- as.list(as.data.frame(apply(ranges, 3, colMeans)))

cc0.1 <- color_to_color(space.one, fun=convertColor)
cc1.1 <- color_to_color(space.one, fun=grDevices1::convertColor)
cc2.1 <- color_to_color(space.one, fun=grDevices2::convertColor)

all.equal(cc0.1, cc1.1)   # TRUE
all.equal(cc1.1, cc2.1)   # TRUE

## 0 Row input, fails in all cases originally except when output is XYZ, in
## which case result is 0 length numeric.  'level-1' and 'level-2' return zero
## row matrices always.

space.zero.row <- sapply(
  space.input, function(x) matrix(numeric(), ncol=3), simplify=FALSE
)
cc0.2 <- color_to_color(space.zero.row, fun=convertColor)
cc1.2 <- color_to_color(space.zero.row, fun=grDevices1::convertColor)
cc2.2 <- color_to_color(space.zero.row, fun=grDevices2::convertColor)

matrix.equal(cc0.2, cc1.2)
cc0.2[['sRGB', 'XYZ']]    # 0 vector
cc1.2[['sRGB', 'XYZ']]    # 0 row matrix
matrix.identical(cc1.2, cc2.2)

## 0 length, All errors on both sides

space.zero <- sapply(space.input, function(x) numeric(), simplify=FALSE)
cc0.3 <- color_to_color(space.zero, fun=convertColor)
cc1.3 <- color_to_color(space.zero, fun=grDevices1::convertColor)
cc2.3 <- color_to_color(space.zero, fun=grDevices1::convertColor)

all.equal(cc0.3, cc1.3)
all.equal(cc0.3, cc2.3)

## bad length (not same warnings, but same results)

space.bad.len <- sapply(space.input, function(x) numeric(4), simplify=FALSE)
cc0.4 <- color_to_color(space.bad.len, fun=convertColor)
cc1.4 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)
cc2.4 <- color_to_color(space.bad.len, fun=grDevices2::convertColor)

all.equal(cc0.4, cc1.4)
all.equal(cc0.4, cc2.4)

## wrong type of input, errors all around for both methods

space.bad.len <- sapply(space.input, function(x) character(3), simplify=FALSE)
cc0.5 <- color_to_color(space.bad.len, fun=convertColor)
cc1.5 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)
cc2.5 <- color_to_color(space.bad.len, fun=grDevices2::convertColor)

all.equal(cc0.5, cc1.5)
all.equal(cc0.5, cc2.5)

## NAs, level-0 fails for any thing from "Lab" or "Luv", or to "Luv", whereas
## level-1 works for all.  For the stuff that works for level-0, those are
## all.equal with level-1.

space.na <- interpolate_space(ranges, steps=2, na=TRUE, expand=numeric())
cc0.6 <- color_to_color(space.na, fun=convertColor)
cc1.6 <- color_to_color(space.na, fun=grDevices1::convertColor)
cc2.6 <- color_to_color(space.na, fun=grDevices2::convertColor)

matrix.identical(cc0.6, cc1.6) | cc0.6 == 'error'
matrix.equal(cc1.6, cc2.6) # small num difference makes them non-identical

## NaN, same as NA results for level-0, level-1

space.nan <- interpolate_space(ranges, steps=2, nan=TRUE, expand=numeric())
cc0.7 <- color_to_color(space.nan, fun=convertColor)
cc1.7 <- color_to_color(space.nan, fun=grDevices1::convertColor)
cc2.7 <- color_to_color(space.nan, fun=grDevices2::convertColor)

matrix.identical(cc0.7, cc1.7) | cc0.7 == 'error'
# level-2 preserves NaNs instead of turning them to NAs.
matrix.identical(cc1.7, cc2.7)
matrix.equal(cc1.7, cc2.7)

## Inf, fails for calculations involving from "Lab" or to "Luv" for level-0, but
## works for level-1.  For those were both work, results are all equal.

space.inf <- interpolate_space(ranges, steps=2, inf=TRUE, expand=numeric())
cc0.8 <- color_to_color(space.inf, fun=convertColor)
cc1.8 <- color_to_color(space.inf, fun=grDevices1::convertColor)
cc2.8 <- color_to_color(space.inf, fun=grDevices2::convertColor)

matrix.identical(cc0.8, cc1.8) | cc0.8 == 'error'
matrix.equal(cc1.8, cc2.8)

# - Other Inputs ---------------------------------------------------------------

## These should not be affected by our changes, but checking nothing is broken

all.equal(
  convertColor(c(30, 20, -20), 'Lab', 'Luv', 'D65', 'E'),
  grDevices2::convertColor(c(30, 20, -20), 'Lab', 'Luv', 'D65', 'E')
)
identical(
  convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=NA),
  grDevices2::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=NA)
)
identical(
  convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=TRUE),
  grDevices2::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=TRUE)
)

# - Performance ----------------------------------------------------------------

stop('performance section best run interactively')

cc0t <- color_to_color(space.input, fun=convertColor, time=5)
cc1t <- color_to_color(space.input, fun=grDevices1::convertColor, time=5)
cc2t <- color_to_color(space.input, fun=grDevices2::convertColor, time=5)

cc0t/cc1t  # level-1 performance improvement
cc0t/cc2t  # level-2 performance improvement

## try smaller matrices to make sure performance is not degraded.

space.ten <- lapply(space.input, function(x) x[seq(1, nrow(x), length.out=10),])

cc0t <- color_to_color(space.ten, fun=convertColor, time=100)
cc1t <- color_to_color(space.ten, fun=grDevices1::convertColor, time=100)
cc2t <- color_to_color(space.ten, fun=grDevices2::convertColor, time=100)

cc0t/cc1t  # level-1 performance improvement
cc0t/cc2t  # level-2 performance improvement

## colorRamp

clrs <-  c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
ramp0 <- colorRamp(clrs, space='Lab')
ramp2 <- grDevices2::colorRamp(clrs, space='Lab')

vals <- (0:1e4) / 1e4
microbenchmark::microbenchmark(ramp0(vals), ramp2(vals), times=5)

# - Visualize ------------------------------------------------------------------

## Just to confirm the color coverage is reasonable.

stop('visualize section best run interactively')

if(require(scales)) {
  rgb.mx <- cc2.0[['CIE RGB', 'sRGB']]
  rgb.mx.2 <-
    rgb.mx[order(rgb.mx[,1], rgb.mx[,2], rgb.mx[,3], decreasing=TRUE),]
  scales::show_col(rgb(rgb.mx.2), labels=FALSE)
}


