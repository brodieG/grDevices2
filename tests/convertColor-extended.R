## For testing simplicity we made a copy of `grDevices` that contains just the
## color conversion functions and called it `grDevices0`.  If you have a recent
## version of r-devel (>= r75340) you can just use `grDevices` directly.

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

if(!require(microbenchmark)) warning("Timed tests require microbenchmark")

source(
  file.path(system.file(package='grDevices2a'), 'tests', 'extra', 'utils.R')
  # file.path('extra', 'utils.R')
)

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

cc0.0 <- color_to_color(space.input, fun=grDevices0::convertColor)
cc1.0 <- color_to_color(space.input, fun=grDevices1::convertColor)
cc2.0 <- color_to_color(space.input, fun=grDevices2a::convertColor)

## color_to_color produces list matrices with the results of the color
## conversions.  Here we show what it looks like, and how to access elements.
## Note that color_to_color drops column names by default as these are changed
## by the patches.

cc0.0
head(cc0.0[['sRGB', 'Lab']])

## Confirm all.equal

all.equal(cc0.0, cc1.0)   # TRUE
all.equal(cc0.0, cc2.0)   # TRUE

## Single color, pick midpoint of ranges

space.one <- as.list(as.data.frame(apply(ranges, 3, colMeans)))

cc0.1 <- color_to_color(space.one, fun=grDevices0::convertColor)
cc1.1 <- color_to_color(space.one, fun=grDevices1::convertColor)
cc2.1 <- color_to_color(space.one, fun=grDevices2a::convertColor)

stopifnot(all.equal(cc0.1, cc1.1))   # TRUE
stopifnot(all.equal(cc1.1, cc2.1))   # TRUE

## 0 Row input, fails in all cases originally except when output is XYZ, in
## which case result is 0 length numeric.  'level-1' and 'level-2' return zero
## row matrices always.

space.zero.row <- sapply(
  space.input, function(x) matrix(numeric(), ncol=3), simplify=FALSE
)
cc0.2 <- color_to_color(space.zero.row, fun=grDevices0::convertColor)
cc1.2 <- color_to_color(space.zero.row, fun=grDevices1::convertColor)
cc2.2 <- color_to_color(space.zero.row, fun=grDevices2a::convertColor)

## matrix.equal make it easy to see which elements are not all.equal

matrix.equal(cc0.2, cc1.2)

cc0.2[['sRGB', 'XYZ']]    # 0 vector
cc1.2[['sRGB', 'XYZ']]    # 0 row matrix

## In most cases level-2 patch is identical to level-1

matrix.identical(cc1.2, cc2.2)
stopifnot(matrix.equal(cc1.2, cc2.2))

## 0 length, All errors on both sides

space.zero <- sapply(space.input, function(x) numeric(), simplify=FALSE)
cc0.3 <- color_to_color(space.zero, fun=grDevices0::convertColor)
cc1.3 <- color_to_color(space.zero, fun=grDevices1::convertColor)
cc2.3 <- color_to_color(space.zero, fun=grDevices2::convertColor)

stopifnot(all.equal(cc0.3, cc1.3))
stopifnot(all.equal(cc0.3, cc2.3))

## bad length (not same warnings, but same results)

space.bad.len <- sapply(space.input, function(x) numeric(4), simplify=FALSE)
cc0.4 <- color_to_color(space.bad.len, fun=grDevices0::convertColor)
cc1.4 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)
cc2.4 <- color_to_color(space.bad.len, fun=grDevices2a::convertColor)

stopifnot(all.equal(cc0.4, cc1.4))
stopifnot(all.equal(cc0.4, cc2.4))

## wrong type of input, errors all around for both methods

space.bad.len <- sapply(space.input, function(x) character(3), simplify=FALSE)
cc0.5 <- color_to_color(space.bad.len, fun=grDevices0::convertColor)
cc1.5 <- color_to_color(space.bad.len, fun=grDevices1::convertColor)
cc2.5 <- color_to_color(space.bad.len, fun=grDevices2a::convertColor)

stopifnot(all.equal(cc0.5, cc1.5))
stopifnot(all.equal(cc0.5, cc2.5))

## NAs, level-0 fails for any thing from "Lab" or "Luv", or to "Luv", whereas
## level-1 works for all.  For the stuff that works for level-0, those are
## all.equal with level-1.

space.na <- interpolate_space(ranges, steps=2, na=TRUE, expand=numeric())
cc0.6 <- color_to_color(space.na, fun=grDevices0::convertColor)
cc1.6 <- color_to_color(space.na, fun=grDevices1::convertColor)
cc2.6 <- color_to_color(space.na, fun=grDevices2a::convertColor)

cc0.6

stopifnot(matrix.identical(cc0.6, cc1.6) | cc0.6 == 'error')
# small num difference makes these non-identical
stopifnot(matrix.equal(cc1.6, cc2.6))

## NaN, same as NA results for level-0, level-1

space.nan <- interpolate_space(ranges, steps=2, nan=TRUE, expand=numeric())
cc0.7 <- color_to_color(space.nan, fun=grDevices0::convertColor)
cc1.7 <- color_to_color(space.nan, fun=grDevices1::convertColor)
cc2.7 <- color_to_color(space.nan, fun=grDevices2a::convertColor)

matrix.identical(cc0.7, cc1.7) | cc0.7 == 'error'
stopifnot(matrix.identical(cc0.7, cc1.7) | cc0.7 == 'error')
# level-2 preserves NaNs instead of turning them to NAs.
matrix.identical(cc1.7, cc2.7)
stopifnot(matrix.equal(cc1.7, cc2.7))

head(cc1.7[['sRGB', 'Lab']])
head(cc2.7[['sRGB', 'Lab']])

## Inf, fails for calculations involving from "Lab" or to "Luv" for level-0, but
## works for level-1.  For those were both work, results are all equal.

space.inf <- interpolate_space(ranges, steps=2, inf=TRUE, expand=numeric())
cc0.8 <- color_to_color(space.inf, fun=grDevices0::convertColor)
cc1.8 <- color_to_color(space.inf, fun=grDevices1::convertColor)
cc2.8 <- color_to_color(space.inf, fun=grDevices2a::convertColor)

stopifnot(matrix.identical(cc0.8, cc1.8) | cc0.8 == 'error')
stopifnot(matrix.equal(cc1.8, cc2.8))

# - Other Inputs ---------------------------------------------------------------

## These should not be affected by our changes, but checking nothing is broken

stopifnot(
  all.equal(
    grDevices0::convertColor(c(30, 20, -20), 'Lab', 'Luv', 'D65', 'E'),
    grDevices2a::convertColor(c(30, 20, -20), 'Lab', 'Luv', 'D65', 'E')
) )
stopifnot(
  identical(
    grDevices0::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=NA),
    grDevices2a::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=NA)
) )
stopifnot(
  identical(
    grDevices0::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=TRUE),
    grDevices2a::convertColor(c(.5, 1, 1), 'XYZ', 'sRGB', clip=TRUE)
) )
## double check that gamma conversions are working; this should already be
## covered from previous tests, but can't see it in `covr` because of
## https://github.com/r-lib/covr/issues/113

srgb0 <- grDevices0::make.rgb(
  red = c(0.6400, 0.3300),
  green = c(0.3000,0.6000),
  blue = c(0.1500,0.0600), gamma = "sRGB",
  white = "D65", name = "sRGB"
)
srgb2a <- grDevices2a::make.rgb(
  red = c(0.6400, 0.3300),
  green = c(0.3000,0.6000),
  blue = c(0.1500,0.0600), gamma = "sRGB",
  white = "D65", name = "sRGB"
)
stopifnot(
  identical(
    grDevices0::convertColor(space.input$sRGB, srgb0, 'XYZ'),
    grDevices2a::convertColor(space.input$sRGB, srgb2a, 'XYZ')
) )
stopifnot(
  identical(
    grDevices0::convertColor(space.input$XYZ, 'XYZ', srgb0),
    grDevices2a::convertColor(space.input$XYZ, 'XYZ', srgb2a)
) )

print(srgb2a)
print(grDevices2a::colorspaces$Lab)

# - Examples -------------------------------------------------------------------

## make.rgb

cols <- t(col2rgb(palette()))
pal0 <- grDevices0::make.rgb(red =   c(0.6400, 0.3300),
                            green = c(0.2900, 0.6000),
                            blue =  c(0.1500, 0.0600),
                            name = "PAL/SECAM RGB")
pal2a <- grDevices2a::make.rgb(red =   c(0.6400, 0.3300),
                               green = c(0.2900, 0.6000),
                               blue =  c(0.1500, 0.0600),
                               name = "PAL/SECAM RGB")
stopifnot(
  all.equal(
    grDevices0::convertColor(cols, from = "sRGB", to = pal0, scale.in = 255),
    grDevices2a::convertColor(cols, from = "sRGB", to = pal2a, scale.in = 255)
) )

hexcolor0 <- grDevices0::colorConverter(toXYZ = function(hex, ...) {
                            rgb <- t(col2rgb(hex))/255
                            grDevices0::colorspaces$sRGB$toXYZ(rgb, ...) },
                           fromXYZ = function(xyz, ...) {
                              rgb <- grDevices0::colorspaces$sRGB$fromXYZ(
                                xyz, ...)
                              rgb <- round(rgb, 5)
                              if (min(rgb) < 0 || max(rgb) > 1)
                                   as.character(NA)
                              else rgb(rgb[1], rgb[2], rgb[3])},
                           white = "D65", name = "#rrggbb")
hexcolor2a <- grDevices2a::colorConverter(toXYZ = function(hex, ...) {
                            rgb <- t(col2rgb(hex))/255
                            grDevices2a::colorspaces$sRGB$toXYZ(rgb, ...) },
                           fromXYZ = function(xyz, ...) {
                              rgb <- grDevices2a::colorspaces$sRGB$fromXYZ(
                                xyz, ...)
                              rgb <- round(rgb, 5)
                              if (min(rgb) < 0 || max(rgb) > 1)
                                   as.character(NA)
                              else rgb(rgb[1], rgb[2], rgb[3])},
                           white = "D65", name = "#rrggbb")

stopifnot(
  all.equal(
    luv <-
      grDevices0::convertColor(cols, from = "sRGB", to = "Luv", scale.in = 255),
    grDevices2a::convertColor(cols, from = "sRGB", to = "Luv", scale.in = 255)
) )
hex <-
  grDevices0::convertColor(luv, from = "Luv",  to = hexcolor0, scale.out = NULL)

## convertColor now always returns matrices

stopifnot(
  all.equal(
    matrix(hex),
    grDevices2a::convertColor(
      luv, from = "Luv",  to = hexcolor2a, scale.out = NULL
    )
) )
## must make hex a matrix before using it
cc <- round(
   grDevices0::convertColor(
      as.matrix(hex), from = hexcolor0, to = "sRGB", scale.in = NULL,
      scale.out = 255
) )
cc2a <- round(
  grDevices2a::convertColor(
    as.matrix(hex), from = hexcolor2a, to = "sRGB", scale.in = NULL,
    scale.out = 255
) )
stopifnot(all.equal(cc, cc2a))
stopifnot(cc == cols)

hexcolorv <- grDevices2a::colorConverter(
  toXYZ = function(hex, ...) {
    rgb <- t(col2rgb(hex))/255
    grDevices2a::colorspaces$sRGB$toXYZ(rgb, ...)
  },
  fromXYZ = function(xyz, ...) {
      rgb <- grDevices2a::colorspaces$sRGB$fromXYZ(xyz, ...)
      rgb <- round(rgb, 5)
      oob <-
        pmin(rgb[,1],rgb[,2],rgb[,3]) < 0 | pmax(rgb[,1],rgb[,2],rgb[,3]) > 0
      res <- rep(NA_character_, nrow(rgb))
      res[!oob] <- rgb(rgb[!oob,,drop=FALSE])},
   white = "D65", name = "#rrggbb",
   vectorized=TRUE
 )
cc2av <- round(
  grDevices2a::convertColor(
    as.matrix(hex), from = hexcolorv, to = "sRGB", scale.in = NULL,
    scale.out = 255
) )
stopifnot(cc2av == cols)

# - More Vectorization tests ---------------------------------------------------

## Test that auto-vectorization of old converters works by creating new
## converters from the old ones

spaces.conv <- lapply(
  grDevices0::colorspaces,
  function(x) {
    if(is.null(x[['white']])) x[['white']] <- x[['reference.white']]
    do.call(
      grDevices2a::colorConverter,
      unname(x[c('toXYZ', 'fromXYZ', 'name', 'white')])
    )
  }
)
cols <- t(col2rgb(palette()))/255 * .7 + .15
XYZ <- grDevices2a::convertColor(cols, 'sRGB', 'XYZ')
fromXYZ <- vapply(
  spaces.conv, grDevices2a::convertColor, FUN.VALUE=XYZ,
  from=spaces.conv[['XYZ']], color=XYZ, clip=NA
)

## Back to XYZ, delta to original XYZ should be close to zero

tol <- 1e-5
toXYZ <- vapply(
  dimnames(fromXYZ)[[3]],
  function(x) {
    all(
      abs(
        grDevices2a:::convertColor(
          fromXYZ[,,x], from=x, to=spaces.conv[['XYZ']]
        ) - XYZ
      ) < tol
    )
  },
  logical(1)
)
stopifnot(all(toXYZ))

## Compare to same version vectorized, but only do one leg since
## we have the check above
##
## Note: we need clip=FALSE b/c the new color converters are not marked as RGB
## even when they are RGB, which changes clip behavior.

cols1 <- t(col2rgb(rainbow(1e3))) / 255 * .7 + .15
XYZ1 <- grDevices2a::convertColor(cols1, 'sRGB', 'XYZ')
system.time(
  fromXYZ1 <- lapply(
    spaces.conv, grDevices2a::convertColor,
    from=spaces.conv[['XYZ']], color=XYZ1, clip=FALSE
  )
)
system.time(
  fromXYZ2 <- lapply(
    grDevices2a::colorspaces, grDevices2a::convertColor,
    from='XYZ', color=XYZ1, clip=FALSE
  )
)
## round to 5 due to RGB colorspaces that we regenerated as normal
## colorspaces

fromXYZ1 <- lapply(fromXYZ1, round, 5)
fromXYZ2 <- lapply(fromXYZ2, round, 5)
# Map(all.equal, fromXYZ1, fromXYZ2)
stopifnot(all.equal(fromXYZ1, fromXYZ2, check.attributes=FALSE))

## Test corner cases; the zero rows only work with new internally
## vectorized method

# try(
#   fromXYZ0.0 <- lapply(
#     grDevices0::colorspaces, grDevices0::convertColor,
#     from='XYZ', color=XYZ1[0,], clip=FALSE
# ) )
# try(
#   fromXYZ1.0 <- lapply(
#     spaces.conv, grDevices2a::convertColor,
#     from=spaces.conv[['XYZ']], color=XYZ1[0,], clip=FALSE
# ) )
# fromXYZ2.0 <- lapply(
#   grDevices2a::colorspaces, grDevices2a::convertColor,
#   from='XYZ', color=XYZ1[0,], clip=FALSE
# )

fromXYZ1.1 <- lapply(
  spaces.conv, grDevices2a::convertColor,
  from=spaces.conv[['XYZ']], color=XYZ1[1,], clip=FALSE
)
fromXYZ2.1 <- lapply(
  grDevices2a::colorspaces, grDevices2a::convertColor,
  from='XYZ', color=XYZ1[1,], clip=FALSE
)
fromXYZ1.1 <- lapply(fromXYZ1, round, 5)
fromXYZ2.1 <- lapply(fromXYZ2, round, 5)
stopifnot(all.equal(fromXYZ1.1, fromXYZ2.1, check.attributes=FALSE))

# - Performance ----------------------------------------------------------------

if(FALSE) {
  cc0t <- color_to_color(space.input, fun=grDevices0::convertColor, time=5)
  cc1t <- color_to_color(space.input, fun=grDevices1::convertColor, time=5)
  cc2t <- color_to_color(space.input, fun=grDevices2a::convertColor, time=5)

  cc0t/cc1t  # level-1 performance improvement
  cc0t/cc2t  # level-2 performance improvement

  ## try smaller matrices to make sure performance is not degraded.

  space.ten <-
    lapply(space.input, function(x) x[seq(1, nrow(x), length.out=10),])

  cc0t.10 <- color_to_color(space.ten, fun=grDevices0::convertColor, time=100)
  cc1t.10 <- color_to_color(space.ten, fun=grDevices1::convertColor, time=100)
  cc2t.10 <- color_to_color(space.ten, fun=grDevices2a::convertColor, time=100)

  cc0t.10/cc1t.10  # level-1 performance improvement
  cc0t.10/cc2t.10  # level-2 performance improvement

  ## colorRamp

  clrs <-  c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  ramp0 <- colorRamp(clrs, space='Lab')
  ramp2 <- grDevices2a::colorRamp(clrs, space='Lab')

  vals <- (0:1e4) / 1e4
  microbenchmark::microbenchmark(ramp0(vals), ramp2(vals), times=5)
}

# - Visualize ------------------------------------------------------------------

## Just to confirm the color coverage is reasonable.

if(FALSE && require(scales)) {
  rgb.mx <- cc2.0[['CIE RGB', 'sRGB']]
  rgb.mx.2 <-
    rgb.mx[order(rgb.mx[,1], rgb.mx[,2], rgb.mx[,3], decreasing=TRUE),]
  scales::show_col(rgb(rgb.mx.2), labels=FALSE)
}

