# Adjust colors

Adjust colors

## Usage

``` r
adjust_colors(
  plot,
  new_colors = NULL,
  saturation = 1,
  labels = tidyplot_parse_labels(),
  downsample = c("evenly", "first", "last", "middle"),
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- new_colors:

  A character vector of new hex colors to use. Can be a named character
  vector of hex colors to assign certain data labels to specific colors.

- saturation:

  A `number` between `0` and `1` for the color saturation of an object.
  A value of `0` is completely desaturated (white), `1` is the original
  color.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - `waiver()` for the default labels computed by the transformation
    object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- downsample:

  If too many colors are provided, whether to downsample `evenly`, or
  use the `first`, the `last` or the `middle` colors of the color
  vector. Defaults to `evenly`.

- ...:

  Arguments passed on to the ggplot2 `scale` function.

## Value

A `tidyplot` object.

## See also

[`colors_discrete_friendly()`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md),
[`colors_continuous_viridis()`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md),
[`colors_diverging_blue2brown()`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md),
and
[`new_color_scheme()`](https://jbengler.github.io/tidyplots/reference/new_color_scheme.md)

## Examples

``` r
# Plot without adjustments
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


# Provide hex colors
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_colors(new_colors = c("#644296","#F08533","#3B78B0", "#D1352C"))


# Provide discrete color scheme
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_colors(new_colors = colors_discrete_seaside)


# Provide named vector
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_colors(new_colors = c(
    "A" = "pink",
    "B" = "purple",
    "C" = "grey",
    "D" = "blue"))


# Provide continuous color scheme
climate |>
  tidyplot(x = month, y = year, color = max_temperature) |>
  add_heatmap() |>
  adjust_colors(new_colors = colors_continuous_turbo)

```
