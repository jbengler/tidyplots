# Remove plot area padding

Remove plot area padding

## Usage

``` r
remove_padding(plot, force_continuous = FALSE)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- force_continuous:

  Whether to force the axis to be continuous. Defaults to `FALSE`.

## Value

A `tidyplot` object.

## Examples

``` r
# Before removing
animals |>
  tidyplot(x = weight, y = speed, color = family) |>
  add_data_points()


# After removing
animals |>
  tidyplot(x = weight, y = speed, color = family) |>
  add_data_points() |>
  remove_padding()

```
