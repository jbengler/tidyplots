# Add violin plot

Add violin plot

## Usage

``` r
add_violin(
  plot,
  dodge_width = NULL,
  alpha = 0.3,
  saturation = 1,
  trim = FALSE,
  linewidth = 0.25,
  scale = "width",
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- dodge_width:

  For adjusting the distance between grouped objects. Defaults to `0.8`
  for plots with at least one discrete axis and `0` for plots with two
  continuous axes.

- alpha:

  A `number` between `0` and `1` for the opacity of an object. A value
  of `0` is completely transparent, `1` is completely opaque.

- saturation:

  A `number` between `0` and `1` for the color saturation of an object.
  A value of `0` is completely desaturated (white), `1` is the original
  color.

- trim:

  If `TRUE` (default), trim the tails of the violins to the range of the
  data. If `FALSE`, don't trim the tails.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- scale:

  if "area" (default), all violins have the same area (before trimming
  the tails). If "count", areas are scaled proportionally to the number
  of observations. If "width", all violins have the same maximum width.

- ...:

  Arguments passed on to the `geom` function.

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin()


# Changing arguments:
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin(saturation = 0.6)


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin(draw_quantiles = c(0.25, 0.5, 0.75))


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin(trim = TRUE)


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_violin(linewidth = 1)

```
