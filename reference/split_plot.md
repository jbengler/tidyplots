# Split plot into multiple subplots

Split plot into multiple subplots

## Usage

``` r
split_plot(
  plot,
  by,
  ncol = NULL,
  nrow = NULL,
  axes = "all",
  scales = "free",
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- by:

  Variable that should be used for splitting.

- ncol, nrow:

  The number of columns and rows per page.

- axes:

  Determines which axes will be drawn in case of fixed scales. When
  `"margins"` (default), axes will be drawn at the exterior margins.
  `"all_x"` and `"all_y"` will draw the respective axes at the interior
  panels too, whereas `"all"` will draw all axes at all panels.

- scales:

  Should scales be fixed (`"fixed"`, the default), free (`"free"`), or
  free in one dimension (`"free_x"`, `"free_y"`)?

- ...:

  Arguments passed on to the `geom` function.

## Value

A `tidyplot` object.

## Examples

``` r
# Before splitting
energy |>
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
  tidyplot(y = energy, color = energy_source) |>
  add_donut() |>
  adjust_size(width = 25, height = 25)


# Split by year
energy |>
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
  tidyplot(y = energy, color = energy_source) |>
  add_donut() |>
  adjust_size(width = 25, height = 25) |>
  split_plot(by = year)


# Change dimensions of subplots
energy |>
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
  tidyplot(y = energy, color = energy_source) |>
  add_donut() |>
  adjust_size(width = 15, height = 15) |>
  split_plot(by = year)


# Spread plots across multiple pages
energy |>
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
  tidyplot(y = energy, color = energy_source) |>
  add_donut() |>
  adjust_size(width = 25, height = 25) |>
  split_plot(by = year, ncol = 2, nrow = 1)
#> ✔ split_plot: split into 4 plots across 2 pages
#> [[1]]

#> 
#> [[2]]

#> 
```
