# Remove plot title or caption

Remove plot title or caption

## Usage

``` r
remove_title(plot)

remove_caption(plot)
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
animals |>
  tidyplot(x = weight, y = speed, color = family) |>
  add_data_points() |>
  add_title("Name of the plot") |>
  add_caption("This is the caption")


# After removing
animals |>
  tidyplot(x = weight, y = speed, color = family) |>
  add_data_points() |>
  add_title("Name of the plot") |>
  add_caption("This is the caption") |>
  remove_title()


animals |>
  tidyplot(x = weight, y = speed, color = family) |>
  add_data_points() |>
  add_title("Name of the plot") |>
  add_caption("This is the caption") |>
  remove_caption()

```
