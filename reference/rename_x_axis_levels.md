# Rename axis or color levels

Rename axis or color levels

## Usage

``` r
rename_x_axis_levels(plot, new_names)

rename_y_axis_levels(plot, new_names)

rename_color_levels(plot, new_names)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- new_names:

  Named character vector in the format c("old1" = "new1", "old2" =
  "new2").

## Value

A `tidyplot` object.

## Examples

``` r
# Before adjustments
study |>
  tidyplot(x = treatment, y = score) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


# Rename x-axis levels
study |>
  tidyplot(x = treatment, y = score) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  rename_x_axis_levels(new_names = c(
    "A" = "This",
    "B" = "is",
    "C" = "totally",
    "D" = "new"))


# Before adjustments
study |>
  tidyplot(x = score, y = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


# Rename y-axis levels
study |>
  tidyplot(x = score, y = treatment) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  rename_y_axis_levels(new_names = c(
    "A" = "This",
    "B" = "is",
    "C" = "totally",
    "D" = "new"))


# Before adjustment
study |>
  tidyplot(x = group, y = score, color = dose) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar()


# Rename color levels
study |>
  tidyplot(x = group, y = score, color = dose) |>
  add_data_points() |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  rename_color_levels(new_names = c(
    "high" = "Sky high",
    "low" = "Deep low"))

```
