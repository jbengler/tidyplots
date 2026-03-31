# Remove plot area clipping

Remove plot area clipping

## Usage

``` r
remove_clipping(plot)
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
spendings |>
  tidyplot(x = amount, y = category, color = category) |>
  add_sum_bar(alpha = 0.2) |>
  add_sum_dash() |>
  add_sum_value(accuracy = 1, color = "black")


# After removing
spendings |>
  tidyplot(x = amount, y = category, color = category) |>
  add_sum_bar(alpha = 0.2) |>
  add_sum_dash() |>
  add_sum_value(accuracy = 1, color = "black") |>
  remove_clipping()

```
