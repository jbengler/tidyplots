# Split plot into multiple subplots

Split plot into multiple subplots

## Usage

``` r
split_plot(
  plot,
  by,
  ncol = NULL,
  nrow = NULL,
  byrow = NULL,
  guides = "collect",
  tag_level = NULL,
  design = NULL
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

- byrow:

  Analogous to `byrow` in
  [matrix()](https://rdrr.io/r/base/matrix.html). If `FALSE` the plots
  will be filled in in column-major order

- guides:

  A string specifying how guides should be treated in the layout.
  `'collect'` will collect guides below to the given nesting level,
  removing duplicates. `'keep'` will stop collection at this level and
  let guides be placed alongside their plot. `auto` will allow guides to
  be collected if a upper level tries, but place them alongside the plot
  if not. If you modify default guide "position" with
  [theme(legend.position=...)](https://ggplot2.tidyverse.org/reference/theme.html)
  while also collecting guides you must apply that change to the overall
  patchwork (see example).

- tag_level:

  A string (`'keep'` or `'new'`) to indicate how auto-tagging should
  behave. See
  [`plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

- design:

  Specification of the location of areas in the layout. Can either be
  specified as a text string or by concatenating calls to
  [`area()`](https://patchwork.data-imaginist.com/reference/area.html)
  together. See the examples for further information on use.

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
#> ✔ split_plot: split into 4 plots across 1 page


# Change dimensions of subplots
energy |>
  dplyr::filter(year %in% c(2005, 2010, 2015, 2020)) |>
  tidyplot(y = energy, color = energy_source) |>
  add_donut() |>
  adjust_size(width = 15, height = 15) |>
  split_plot(by = year)
#> ✔ split_plot: split into 4 plots across 1 page


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
