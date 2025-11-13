# Adjust legend

Adjust legend

## Usage

``` r
adjust_legend_title(
  plot,
  title = ggplot2::waiver(),
  fontsize = NULL,
  family = NULL,
  face = NULL,
  color = "black",
  ...
)

adjust_legend_position(plot, position = "right")
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- title:

  Legend title.

- fontsize:

  Font size in points. Defaults to `7`.

- family:

  The typeface to use. The validity of this value will depend on the
  graphics device being used for rendering the plot. See [the
  systemfonts
  vignette](https://systemfonts.r-lib.org/articles/systemfonts.html) for
  guidance on the best way to access fonts installed on your computer.
  The values `"sans"`, `"serif"`, and `"mono"` should always be valid
  and will select the default typeface for the respective styles.
  However, what is considered default is dependant on the graphics
  device and the operating system.

- face:

  Font face ("plain", "italic", "bold", "bold.italic")

- color:

  A hex color for the stroke color. For example, `"#FFFFFF"` for white.

- ...:

  Arguments passed on to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html).

- position:

  The position of the legend. Can be one of
  `c("right", "left", "bottom", "top", "none")`. Defaults to `"right"`.

## Value

A `tidyplot` object.

## Details

- The `title` argument of `adjust_legend_title()` supports [plotmath
  expressions](https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath)
  to include special characters. See examples and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#special-characters).

## Examples

``` r
# Plot without adjustments
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


# New title
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_title("My new legend title")


# New title with plotmath expression
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_title("$E==m*c^{2}$")


# Alternative legend positions
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_position("left")


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_position("top")


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_position("bottom")


# `position = "none"` hides the legend
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  adjust_legend_position("none")

```
