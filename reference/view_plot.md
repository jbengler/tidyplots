# View plot on screen

View plot on screen

## Usage

``` r
view_plot(plot, data = all_rows(), title = ggplot2::waiver(), ...)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- data:

  The data to be displayed in this layer. There are three options:

  - If
    [`all_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
    (the default) the complete dataset is displayed.

  - A `function` to subset the plot data. See
    [`filter_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
    and friends.

  - A `data.frame` to override the plot data.

- title:

  Plot title.

- ...:

  Arguments passed on to [`print()`](https://rdrr.io/r/base/print.html).

## Value

A `tidyplot` object.

## Details

- `view_plot()` supports data subsetting. See examples and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).

## Examples

``` r
# View intermediate stages on screen
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  view_plot(title = "Before changing color scheme") |>
  adjust_colors(colors_discrete_seaside) |>
  view_plot(title = "After changing color scheme")



# View data subsets on screen
gene_expression |>
  tidyplot(x = condition, y = expression, color = sample_type) |>
  add_mean_dash() |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  view_plot(data = filter_rows(external_gene_name == "Apol6"),
    title = "Apol6") |>
  view_plot(data = filter_rows(external_gene_name == "Bsn"),
    title = "Bsn")


```
