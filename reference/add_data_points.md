# Add data points

Add data points

## Usage

``` r
add_data_points(
  plot,
  data = all_rows(),
  shape = 19,
  size = 1,
  white_border = FALSE,
  dodge_width = NULL,
  preserve = "total",
  rasterize = FALSE,
  rasterize_dpi = 300,
  ...
)

add_data_points_jitter(
  plot,
  data = all_rows(),
  shape = 19,
  size = 1,
  white_border = FALSE,
  dodge_width = NULL,
  jitter_width = 0.2,
  jitter_height = 0,
  preserve = "total",
  rasterize = FALSE,
  rasterize_dpi = 300,
  ...
)

add_data_points_beeswarm(
  plot,
  data = all_rows(),
  shape = 19,
  size = 1,
  white_border = FALSE,
  cex = 3,
  corral = "wrap",
  corral.width = 0.5,
  dodge_width = NULL,
  preserve = "total",
  rasterize = FALSE,
  rasterize_dpi = 300,
  ...
)
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

- shape:

  An `integer` between `0` and `24`, representing the shape of the plot
  symbol.

  ![](figures/unnamed-chunk-1-1.png)

- size:

  A `number` representing the size of the plot symbol. Typical values
  range between `1` and `3`.

- white_border:

  Whether to include a white border around data points. Defaults to
  `FALSE`.

- dodge_width:

  For adjusting the distance between grouped objects. Defaults to `0.8`
  for plots with at least one discrete axis and `0` for plots with two
  continuous axes.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- rasterize:

  If `FALSE` (the default) the layer will be constructed of vector
  shapes. If `TRUE` the layer will be rasterized to a pixel image. This
  can be useful when plotting many individual objects (1,000 or more)
  compromises the performance of the generated PDF file.

- rasterize_dpi:

  The resolution in dots per inch (dpi) used for rastering the layer if
  `rasterize` is `TRUE`. The default is `300` dpi.

- ...:

  Arguments passed on to the `geom` function.

- jitter_width:

  Amount of random noise to be added to the horizontal position of the
  of the data points. This can be useful to deal with overplotting.
  Typical values range between `0` and `1`.

- jitter_height:

  Amount of random noise to be added to the vertical position of the of
  the data points. This can be useful to deal with overplotting. Typical
  values range between `0` and `1`.

- cex:

  Scaling for adjusting point spacing (see
  [`beeswarm::swarmx()`](https://rdrr.io/pkg/beeswarm/man/swarmx.html)).
  Values between 1 (default) and 3 tend to work best.

- corral:

  `string`. Method used to adjust points that would be placed to wide
  horizontally, default is `"none"`. See details below.

- corral.width:

  `numeric`. Width of the corral, default is `0.9`.

## Value

A `tidyplot` object.

## Details

- `add_data_points_beeswarm()` is based on
  [`ggbeeswarm::geom_beeswarm()`](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html).
  Check there for additional arguments.

- `add_data_points()` and friends support rasterization. See examples
  and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#rasterization).

- `add_data_points()` and friends support data subsetting. See examples
  and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).

## Examples

``` r
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_jitter()


study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_beeswarm()


# Changing arguments
study |>
  tidyplot(x = treatment, y = score, color = treatment) |>
  add_data_points_jitter(jitter_width = 1)


animals |>
  tidyplot(x = weight, y = size) |>
  add_data_points(white_border = TRUE)


animals |>
  tidyplot(x = weight, y = size) |>
  add_data_points(alpha = 0.4)


# Rasterization
animals |>
  tidyplot(x = weight, y = size) |>
  add_data_points(rasterize = TRUE, rasterize_dpi = 50)


# Data subsetting
animals |>
  tidyplot(x = weight, y = size) |>
  add_data_points() |>
  add_data_points(data = filter_rows(size > 300), color = "red")

```
