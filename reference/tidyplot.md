# Create a new tidyplot

Create a new tidyplot

## Usage

``` r
tidyplot(
  data,
  ...,
  width = NULL,
  height = NULL,
  unit = NULL,
  dodge_width = NULL,
  my_style = NULL
)
```

## Arguments

- data:

  A tidy `data.frame` to use for plotting.

- ...:

  Mappings for the `x` axis, `y` axis and `color`, see examples.
  Additional argument are passed to
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- width:

  Width of the plot area. The default (`NULL`) retrieves the setting
  from the [tidyplots
  options](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md),
  which defaults to `50`.

- height:

  Height of the plot area. The default (`NULL`) retrieves the setting
  from the [tidyplots
  options](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md),
  which defaults to `50`.

- unit:

  Unit of the plot area width and height. The default (`NULL`) retrieves
  the setting from the [tidyplots
  options](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md),
  which defaults to `"mm"`.

- dodge_width:

  For adjusting the distance between grouped objects. The default
  (`NULL`) retrieves the setting from the [tidyplots
  options](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md),
  which defaults to `0.8` for plots with at least one discrete axis and
  to `0` for plots with two continuous axes.

- my_style:

  Styling function to apply to the plot. The default (`NULL`) retrieves
  the setting from the [tidyplots
  options](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md),
  which default to no additional styling.

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm()


study |>
  tidyplot(x = group, y = score, color = dose) |>
  add_mean_bar()


# Change plot area size
study |>
  tidyplot(x = treatment, y = score, color = treatment,
    width = 25, height = 25) |>
  add_data_points_beeswarm()


# Change dodge_width
study |>
  tidyplot(x = group, y = score, color = dose, dodge_width = 0.3) |>
  add_mean_bar()

```
