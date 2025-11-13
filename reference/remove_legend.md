# Remove legend or legend title

Remove legend or legend title

## Usage

``` r
remove_legend(plot)

remove_legend_title(plot)
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
  remove_legend_title()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar() |>
  remove_legend()

```
