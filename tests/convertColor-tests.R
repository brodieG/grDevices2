library(grDevices)
cols <- t(col2rgb(palette()))

## One full space1-XYZ-space2 conversion

convertColor(cols, 'sRGB', 'Lab', scale.in=255)

## to XYZ, then to every defined space

XYZ <- convertColor(cols, 'sRGB', 'XYZ', scale.in=255)
fromXYZ <- vapply(
  names(colorspaces), convertColor, FUN.VALUE=XYZ,
  from='XYZ', color=XYZ, clip=NA
)
round(fromXYZ, 3)

## Back to XYZ, delta to original XYZ should be close to zero

tol <- 1e-5
toXYZ <- vapply(
  dimnames(fromXYZ)[[3]],
  function(x) all(abs(convertColor(fromXYZ[,,x], from=x, to='XYZ') - XYZ) < tol)
  logical(a)
)
toXYZ
stopifnot(all(toXYZ | is.na(toXYZ)))

## Test Apple RGB on smaller gamut since it clips

XYZ2 <- XYZ * .9 + .05
stopifnot(
  all(
    abs(
      convertColor(
        convertColor(XYZ2, 'XYZ', 'Apple RGB', clip=NA), 'Apple RGB', 'XYZ'
      ) - XYZ2 < tol
) ) )

