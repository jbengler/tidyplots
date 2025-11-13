# Add line or area

`add_line()` and `add_area()` connect individual data points, which is
rarely needed. In most cases, you are probably looking for
[`add_sum_line()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md),
[`add_mean_line()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md),
[`add_sum_area()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
or
[`add_mean_area()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md).

## Usage

``` r
add_line(
  plot,
  group,
  dodge_width = NULL,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_area(
  plot,
  group,
  dodge_width = NULL,
  linewidth = 0.25,
  alpha = 0.4,
  preserve = "total",
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- group:

  Variable in the dataset to be used for grouping.

- dodge_width:

  For adjusting the distance between grouped objects. Defaults to `0.8`
  for plots with at least one discrete axis and `0` for plots with two
  continuous axes.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- ...:

  Arguments passed on to the `geom` function.

- alpha:

  A `number` between `0` and `1` for the opacity of an object. A value
  of `0` is completely transparent, `1` is completely opaque.

## Value

A `tidyplot` object.

## Examples

``` r
# Paired data points
study |>
  tidyplot(x = treatment, y = score, color = group) |>
  reorder_x_axis_labels("A", "C", "B", "D") |>
  add_data_points() |>
  add_line(group = participant, color = "grey")


study |>
  tidyplot(x = treatment, y = score) |>
  reorder_x_axis_labels("A", "C", "B", "D") |>
  add_data_points() |>
  add_area(group = participant)

```
