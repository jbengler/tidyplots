# Add error bar

- `add_sem_errorbar()` adds the standard error of mean.

- `add_range_errorbar()` adds the range from smallest to largest value.

- `add_sd_errorbar()` adds the standard deviation.

- `add_ci95_errorbar()` adds the 95% confidence interval.

## Usage

``` r
add_sem_errorbar(
  plot,
  dodge_width = NULL,
  width = 0.4,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_range_errorbar(
  plot,
  dodge_width = NULL,
  width = 0.4,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_sd_errorbar(
  plot,
  dodge_width = NULL,
  width = 0.4,
  linewidth = 0.25,
  preserve = "total",
  ...
)

add_ci95_errorbar(
  plot,
  dodge_width = NULL,
  width = 0.4,
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

  Width of the error bar.

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
# Standard error of the mean
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_sem_errorbar()


# Range from minimum to maximum value
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_range_errorbar()


# Standard deviation
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_sd_errorbar()


# 95% confidence interval
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_ci95_errorbar()


# Changing arguments: error bar width
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_sem_errorbar(width = 0.8)


# Changing arguments: error bar line width
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_sem_errorbar(linewidth = 1)

```
