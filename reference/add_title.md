# Add plot title or caption

Add plot title or caption

## Usage

``` r
add_title(plot, title = ggplot2::waiver())

add_caption(plot, caption = ggplot2::waiver())
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- title:

  Title of the plot.

- caption:

  Caption of the plot.

## Value

A `tidyplot` object.

## Details

- `add_title()` and `add_caption()` support [plotmath
  expressions](https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath)
  to include special characters. See examples and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#special-characters).

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score) |>
  add_data_points_beeswarm() |>
  add_title("This is my title")


study |>
  tidyplot(x = treatment, y = score) |>
  add_data_points_beeswarm() |>
  add_caption("This is the fine print in the caption")


# Plotmath expression
study |>
  tidyplot(x = treatment, y = score) |>
  add_data_points_beeswarm() |>
  add_title("$H[2]*O~and~E==m*c^{2}$")

```
