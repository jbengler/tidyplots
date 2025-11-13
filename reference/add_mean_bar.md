# Add mean

Add mean

## Usage

``` r
add_mean_bar(
  plot,
  dodge_width = NULL,
  width = 0.6,
  saturation = 1,
  preserve = "total",
  ...
)

add_mean_dash(
  plot,
  dodge_width = NULL,
  width = 0.6,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_mean_dot(plot, dodge_width = NULL, size = 2, preserve = "total", ...)

add_mean_value(
  plot,
  dodge_width = NULL,
  accuracy = 0.1,
  scale_cut = NULL,
  fontsize = 7,
  extra_padding = 0.15,
  vjust = NULL,
  hjust = NULL,
  preserve = "total",
  ...
)

add_mean_line(
  plot,
  group,
  dodge_width = NULL,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_mean_area(
  plot,
  group,
  dodge_width = NULL,
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

- width:

  Width of the bar.

- saturation:

  A `number` between `0` and `1` for the color saturation of an object.
  A value of `0` is completely desaturated (white), `1` is the original
  color.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- ...:

  Arguments passed on to the `geom` function.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- size:

  A `number` representing the size of the plot symbol. Typical values
  range between `1` and `3`.

- accuracy:

  A number to round to. Use (e.g.) `0.01` to show 2 decimal places of
  precision. If `NULL`, the default, uses a heuristic that should ensure
  breaks have the minimum number of digits needed to show the difference
  between adjacent values.

  Applied to rescaled data.

- scale_cut:

  Scale cut function to be applied. See
  [`scales::cut_short_scale()`](https://scales.r-lib.org/reference/number.html)
  and friends.

- fontsize:

  Font size in points. Defaults to `7`.

- extra_padding:

  Extra padding to create space for the value label.

- vjust:

  Vertical position adjustment of the value label.

- hjust:

  Horizontal position adjustment of the value label.

- group:

  Variable in the dataset to be used for grouping.

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_dash()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_dot()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_value()


study |>
  tidyplot(x = treatment, y = score) |>
  add_mean_line()


study |>
  tidyplot(x = treatment, y = score) |>
  add_mean_area()


# Combination
study |>
  tidyplot(x = treatment, y = score) |>
  add_mean_bar(alpha = 0.4) |>
  add_mean_dash() |>
  add_mean_dot() |>
  add_mean_value() |>
  add_mean_line()


# Changing arguments: alpha
# Makes objects transparent
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  theme_minimal_y() |>
  add_mean_bar(alpha = 0.4)


# Changing arguments: saturation
# Reduces fill color saturation without making the object transparent
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  theme_minimal_y() |>
  add_mean_bar(saturation = 0.3)


# Changing arguments: accuracy
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_value(accuracy = 0.01)


# Changing arguments: fontsize
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_value(fontsize = 10)


# Changing arguments: color
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_value(color = "black")

```
