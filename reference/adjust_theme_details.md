# Adjust theme details

This function is a wrapper around
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
To use the required theme helper functions
[`ggplot2::element_blank()`](https://ggplot2.tidyverse.org/reference/element.html),
[`ggplot2::element_rect()`](https://ggplot2.tidyverse.org/reference/element.html),
[`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html),
and
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
you need to either load the ggplot2 package via
[`library(ggplot2)`](https://ggplot2.tidyverse.org) or use the
`ggplot2::` prefix as shown above.

## Usage

``` r
adjust_theme_details(plot, ...)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- ...:

  Arguments passed on to the `geom` function.

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  adjust_theme_details(plot.background = ggplot2::element_rect(fill = "#FFEBFF"))

```
