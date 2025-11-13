# Add ellipse

Add ellipse

## Usage

``` r
add_ellipse(plot, ...)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- ...:

  Arguments passed on to
  [`ggplot2::stat_ellipse()`](https://ggplot2.tidyverse.org/reference/stat_ellipse.html).

## Value

A `tidyplot` object.

## Examples

``` r
pca |>
  tidyplot(x = pc1, y = pc2, color = group) |>
  add_data_points() |>
  add_ellipse()


pca |>
  tidyplot(x = pc1, y = pc2, color = group) |>
  add_data_points() |>
  add_ellipse(level = 0.75)


pca |>
  tidyplot(x = pc1, y = pc2, color = group) |>
  add_data_points() |>
  add_ellipse(type = "norm")

```
