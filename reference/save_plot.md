# Save plots to file

This function takes a plot or list of plots and writes them to a
(multipage) file.

## Usage

``` r
save_plot(
  plot = ggplot2::last_plot(),
  filename,
  width = NA,
  height = NA,
  units = c("mm", "cm", "in"),
  padding = 0.1,
  multiple_files = FALSE,
  view_plot = TRUE,
  ...
)
```

## Arguments

- plot:

  Plot to save, defaults to last plot displayed.

- filename:

  File name to create on disk.

- width, height:

  Dimensions of the graphic device to save the plot. Defaults to `NA`.
  In case of `NA`, the dimensions are inferred from the incoming `plot`
  object (see Details).

- units:

  Units of length. Defaults to `"mm"`.

- padding:

  Extra space around the saved plot. Defaults to `0.1` meaning 10%.

- multiple_files:

  Whether to save multiple pages as individual files.

- view_plot:

  Whether to view the plot on screen after saving.

- ...:

  Other arguments passed on to the graphics device function, as
  specified by `device`.

## Value

A `tidyplot` object.

## Details

**Handling of file dimensions.** Output file dimensions are determined
according the the following precedence.

1.  The `width` and `height` arguments.

2.  Dimensions inferred from the incoming `plot` object with absolute
    dimensions.

3.  System default device dimensions.

## Examples

``` r
# Save plot to file
study |>
  tidyplot(treatment, score) |>
  add_data_points() |>
  save_plot("single_plot.pdf")
#> ✔ save_plot: saved to single_plot.pdf


# Save intermediate stages to file
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_mean_bar(alpha = 0.4) |>
  add_sem_errorbar() |>
  add_data_points_beeswarm() |>
  save_plot("before.pdf") |>
  adjust_colors(colors_discrete_seaside) |>
  save_plot("after.pdf")
#> ✔ save_plot: saved to before.pdf

#> ✔ save_plot: saved to after.pdf


# \donttest{

# Save multipage PDF file
gene_expression |>
  dplyr::slice_head(n = 160) |>
  tidyplot(group, expression, color = sample_type) |>
  add_data_points() |>
  adjust_size(width = 30, height = 25) |>
  split_plot(by = external_gene_name, nrow = 2, ncol = 2) |>
  save_plot("multipage_plot.pdf")
#> ✔ split_plot: split into 8 plots across 2 pages
#> ✔ save_plot: saved multipage PDF to multipage_plot.pdf
#> [[1]]

#> 
#> [[2]]

#> 

# Save multiple PDF files
gene_expression |>
  dplyr::slice_head(n = 160) |>
  tidyplot(group, expression, color = sample_type) |>
  add_data_points() |>
  adjust_size(width = 30, height = 25) |>
  split_plot(by = external_gene_name, nrow = 2, ncol = 2) |>
  save_plot("plot.pdf", multiple_files = TRUE)
#> ✔ split_plot: split into 8 plots across 2 pages
#> ✔ save_plot: saved multiple plots to plot_1.pdf and plot_2.pdf
#> [[1]]

#> 
#> [[2]]

#> 

# }
```
