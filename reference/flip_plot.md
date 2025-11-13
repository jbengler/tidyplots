# Flip x and y-axis

**\[superseded\]**

This function is superseded because in many cases, `flip_plot()` can
easily be replaced by swapping the `x` and `y` axis. Some plot
components additionally require to set the `orientation` argument to
`"y"`.

## Usage

``` r
flip_plot(plot, ...)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- ...:

  Arguments passed on to
  [`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

## Value

A `tidyplot` object.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  flip_plot()


energy |>
  tidyplot(x = year, y = energy, color = energy_type) |>
  add_barstack_absolute() |>
  flip_plot()


# Better solutions without `flip_plot()`
study |>
  tidyplot(x = score, y = treatment, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


energy |>
  tidyplot(x = energy, y = year, color = energy_type) |>
  add_barstack_absolute(orientation = "y")

```
