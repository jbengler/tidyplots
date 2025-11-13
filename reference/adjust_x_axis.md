# Adjust axes

Adjust axes

## Usage

``` r
adjust_x_axis(
  plot,
  title = ggplot2::waiver(),
  breaks = ggplot2::waiver(),
  labels = NULL,
  limits = NULL,
  padding = c(NA, NA),
  rotate_labels = FALSE,
  transform = "identity",
  cut_short_scale = FALSE,
  force_continuous = FALSE,
  ...
)

adjust_y_axis(
  plot,
  title = ggplot2::waiver(),
  breaks = ggplot2::waiver(),
  labels = NULL,
  limits = NULL,
  padding = c(NA, NA),
  rotate_labels = FALSE,
  transform = "identity",
  cut_short_scale = FALSE,
  force_continuous = FALSE,
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- title:

  Axis title.

- breaks:

  One of:

  - `NULL` for no breaks

  - `waiver()` for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

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

- limits:

  Axis limits. For example, with `limits = c(20, 90)` the axis starts at
  20 and ends at 90.

- padding:

  Extra space between the data points and the axes. Defaults to
  `c(NA, NA)`, which does not change the padding.

- rotate_labels:

  Whether to rotate axis labels. If `TRUE` is set to 45 degrees. You can
  also provide custom degree values, for example, `rotate_labels = 90`.
  Defaults to `FALSE`.

- transform:

  For continuous scales, the name of a transformation object or the
  object itself. Built-in transformations include "asn", "atanh",
  "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
  "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
  "reciprocal", "reverse", "sqrt" and "time".

  A transformation object bundles together a transform, its inverse, and
  methods for generating breaks and labels. Transformation objects are
  defined in the scales package, and are called `transform_<name>`. If
  transformations require arguments, you can call them from the scales
  package, e.g.
  [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
  You can create your own transformation with
  [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

- cut_short_scale:

  Whether to shorten axis labels using `K` for thousand, `M` for
  million, and so on. Defaults to `FALSE`.

- force_continuous:

  Whether to force the axis to be continuous. Defaults to `FALSE`.

- ...:

  Arguments passed on to ggplot2 `scale` function.

## Value

A `tidyplot` object.

## Details

- The `title` argument of `adjust_x_axis()` and `adjust_y_axis()`
  supports [plotmath
  expressions](https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath)
  to include special characters. See examples and [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#special-characters).

## Examples

``` r
# Plot without adjustments
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points()


# New titles
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(title = "My new x-axis title") |>
  adjust_y_axis(title = "My new y-axis title")


# New titles with plotmath expressions
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(title = "$H[2]*O$") |>
  adjust_y_axis(title = "$E==m*c^{2}$")


# Axes limits
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(limits = c(-1000, 4000)) |>
  adjust_y_axis(limits = c(-200, 600))


# Rotate labels
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(rotate_labels = 90) |>
  adjust_y_axis(rotate_labels = 90)


# Increase plot area padding
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(padding = c(0.2, 0.2)) |>
  adjust_y_axis(padding = c(0.2, 0.2))


# Scale transformation
animals |>
  tidyplot(x = weight, y = size, color = family) |>
  add_data_points() |>
  adjust_x_axis(transform = "log10") |>
  adjust_y_axis(transform = "log2")

```
