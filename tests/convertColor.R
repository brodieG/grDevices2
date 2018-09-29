## These tests are not intended to become part of the patch

# - Sample Input Spaces --------------------------------------------------------

source('tests/utils.R')

ranges.raw <- c(
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0,   1,   0,   1,   0,   1,  # rgb*/xyz
  0, 100,-128, 128,-128, 128,  # Lab
  # Luv; supposed to be +- 100, but rgb conv suggests it's wider than that
  0, 100,-180, 180,-180, 180
)
spaces <- c('Apple RGB', 'sRGB', 'CIE RGB', 'XYZ', 'Lab', 'Luv')

ranges <- array(
  ranges.raw, dim=c(2, 3, length(ranges.raw) / (2 * 3)),
  dimnames=list(range=c('lo', 'hi'), NULL, space=spaces)
)
# space.sub <- c('sRGB', 'XYZ', 'Lab', 'Luv')
space.sub <- spaces
ranges.sub <- ranges[,,match(space.sub, space.sub)]

# For each input space, generate permutation of values in range, outside of
# range, along with NA, NaN, Inf, -Inf cases.

space.input <- interpolate_space(ranges.sub, steps=16)
stop('test prep done')

# - Compare Convert Color ------------------------------------------------------

## All colors

cc0.0 <- color_to_color(space.input, fun=grDevices0::convertColor)
cc1.0 <- color_to_color(space.input, fun=grDevices1::convertColor)

all.equal(cc0.0, cc1.0)

## Single color, pick midpoint of ranges

space.one <- as.list(as.data.frame(apply(ranges, 3, colMeans)))

cc0.1 <- color_to_color(space.one, fun=grDevices0::convertColor)
cc1.1 <- color_to_color(space.one, fun=grDevices1::convertColor)

all.equal(cc0.1, cc1.1)

## 0 Row

space.zero.row <- sapply(
  space.input, function(x) matrix(numeric(), ncol=3), simplify=FALSE
)
cc0.2 <- color_to_color(space.zero.row, fun=grDevices0::convertColor)
cc1.2 <- color_to_color(space.zero.row, fun=grDevices1::convertColor)

## 0 length, All errors on both sides

space.zero <- sapply(space.input, function(x) numeric(), simplify=FALSE)
cc0.3 <- color_to_color(space.zero, fun=grDevices0::convertColor)
cc1.3 <- color_to_color(space.zero, fun=grDevices1::convertColor)

all.equal(cc0.3, cc1.3)

## bad length (not same warnings, but same results)

space.bad.len <- sapply(space.input, function(x) numeric(4), simplify=FALSE)
cc0.4 <- color_to_color(space.bad.len, fun=grDevices0::convertColor)
cc1.4 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)

all.equal(cc0.4, cc1.4)

## wrong type of input, errors all around for both methods

space.bad.len <- sapply(space.input, function(x) character(3), simplify=FALSE)
cc0.5 <- color_to_color(space.bad.len, fun=grDevices0::convertColor)
cc1.5 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)

all.equal(cc0.5, cc1.5)

## NAs, level-0 fails for any thing from "Lab" or "Luv", or to "Luv", whereas
## level-0 works for all.  For the stuff that works for level-0, those are
## all.equal with level-1.

space.na <- interpolate_space(ranges.sub, steps=2, na=TRUE, expand=numeric())
cc0.6 <- color_to_color(space.na, fun=grDevices0::convertColor)
cc1.6 <- color_to_color(space.na, fun=grDevices1::convertColor)

all.equal(cc0.6, cc1.6)
matrix.equal(cc0.6, cc1.6) | cc0.6 == 'error'

## NaN, same as NA results for level-0, level-1

space.nan <- interpolate_space(ranges.sub, steps=2, nan=TRUE, expand=numeric())
cc0.7 <- color_to_color(space.nan, fun=grDevices0::convertColor)
cc1.7 <- color_to_color(space.nan, fun=grDevices1::convertColor)

all.equal(cc0.7, cc1.7)
matrix.identical(cc0.7, cc1.7) | cc0.7 == 'error'

## Inf, fails for calculations involving from "Lab" or to "Luv" for level-0, but
## works for level-1.  For those were both work, results are all equal.

space.inf <- interpolate_space(ranges.sub, steps=2, inf=TRUE, expand=numeric())
cc0.8 <- color_to_color(space.inf, fun=grDevices0::convertColor)
cc1.8 <- color_to_color(space.inf, fun=grDevices1::convertColor)

all.equal(cc0.8, cc1.8)
matrix.identical(cc0.8, cc1.8) | cc0.8 == 'error'

# - Performance ----------------------------------------------------------------

cc0t <- color_to_color(space.input, fun=grDevices0::convertColor, time=1)
cc1t <- color_to_color(space.input, fun=grDevices1::convertColor, time=1)


