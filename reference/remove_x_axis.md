# Remove x-axis or parts of it

Remove x-axis or parts of it

## Usage

``` r
remove_x_axis(plot)

remove_x_axis_line(plot)

remove_x_axis_ticks(plot)

remove_x_axis_labels(plot)

remove_x_axis_title(plot)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

## Value

A `tidyplot` object.

## Examples

``` r
# Before removing
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar()


# After removing
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_x_axis_line()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_x_axis_ticks()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_x_axis_labels()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_x_axis_title()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_x_axis()

```
