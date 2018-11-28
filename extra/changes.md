# level-0

# level-1

# level-2

* Inputs that would previously stop with errors or work inconsistently
  now work consistently (e.g. zero-row inputs or inputs containing
  NA/NaN/Inf).
* Column names are consistently set to the color space initials; these
  were previously inconsistently set / mangled by `c`.
* Return is always a matrix, previously color converters that returned 1 length
  colors (e.g. as with the `hexcolor` example in `?make.rgb`) would return a
  vector due to simplification by `apply`.
* Wrap color converters in `apply`, add `vectorized` param to `colorConverter`

