# Add boxplot

Add boxplot

## Usage

``` r
add_boxplot(
  plot,
  dodge_width = NULL,
  alpha = 0.3,
  saturation = 1,
  show_whiskers = TRUE,
  show_outliers = TRUE,
  box_width = 0.6,
  whiskers_width = 0.8,
  outlier.size = 0.5,
  coef = 1.5,
  outlier.shape = 19,
  outlier.alpha = 1,
  linewidth = 0.25,
  preserve = "total",
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

- show_whiskers:

  Whether to show boxplot whiskers. Defaults to `TRUE`.

- show_outliers:

  Whether to show outliers. Defaults to `TRUE`.

- box_width:

  Width of the boxplot. Defaults to `0.6`.

- whiskers_width:

  Width of the whiskers. Defaults to `0.8`.

- outlier.size:

  Size of the outliers. Defaults to `0.5`.

- coef:

  Length of the whiskers as multiple of IQR. Defaults to 1.5.

- outlier.shape:

  Shape of the outliers. Defaults to `19`.

- outlier.alpha:

  Opacity of the outliers. Defaults to `1`.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- ...:

  Arguments passed on to the `geom` function.

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_boxplot()


# Changing arguments:
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_boxplot(show_whiskers = FALSE)


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_boxplot(show_outliers = FALSE)


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_boxplot(box_width = 0.2)


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_boxplot(whiskers_width = 0.2)

```
