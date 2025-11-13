# Add reference lines

Add reference lines

## Usage

``` r
add_reference_lines(
  plot,
  x = NULL,
  y = NULL,
  linetype = "dashed",
  linewidth = 0.25,
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- x:

  Numeric values where the reference lines should meet the x-axis. For
  example, `x = 4` or `x = c(2,3,4)`.

- y:

  Numeric values where the reference lines should meet the y-axis. For
  example, `y = 4` or `y = c(2,3,4)`.

- linetype:

  Either an integer (0-6) or a name (0 = blank, 1 = solid, 2 = dashed, 3
  = dotted, 4 = dotdash, 5 = longdash, 6 = twodash).

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- ...:

  Arguments passed on to the `geom` function.

## Value

A `tidyplot` object.

## Examples

``` r
animals |>
  tidyplot(x = weight, y = speed) |>
   add_reference_lines(x = 4000, y = c(100, 200)) |>
   add_data_points()


animals |>
  tidyplot(x = weight, y = speed) |>
   add_reference_lines(x = 4000, y = c(100, 200), linetype = "dotdash") |>
   add_data_points()

```
