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

space.input <- interpolate_space(ranges.sub, na=FALSE, inf=FALSE, steps=16)

# - Compare Convert Color ------------------------------------------------------

cc0 <- color_to_color(space.input, fun=grDevices0::convertColor)
cc1 <- color_to_color(space.input, fun=grDevices1::convertColor)

grDevices1::convertColor(space.input$sRGB, 'sRGB', 'CIE RGB')
