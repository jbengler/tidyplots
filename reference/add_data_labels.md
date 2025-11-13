# Add data labels

Add data labels

## Usage

``` r
add_data_labels(
  plot,
  label,
  data = all_rows(),
  fontsize = 7,
  dodge_width = NULL,
  jitter_width = 0,
  jitter_height = 0,
  preserve = "total",
  background = FALSE,
  background_color = "#FFFFFF",
  background_alpha = 0.6,
  label_position = c("below", "above", "left", "right", "center"),
  ...
)

add_data_labels_repel(
  plot,
  label,
  data = all_rows(),
  fontsize = 7,
  dodge_width = NULL,
  jitter_width = 0,
  jitter_height = 0,
  preserve = "total",
  segment.size = 0.2,
  box.padding = 0.2,
  max.overlaps = Inf,
  background = FALSE,
  background_color = "#FFFFFF",
  background_alpha = 0.6,
  ...
)
```

## Arguments

- plot:

  A `tidyplot` generated with the function
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md).

- label:

  Variable in the dataset to be used for the text label.

- data:

  The data to be displayed in this layer. There are three options:

  - If
    [`all_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
    (the default) the complete dataset is displayed.

  - A `function` to subset the plot data. See
    [`filter_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
    and friends.

  - A `data.frame` to override the plot data.

- fontsize:

  Font size in points. Defaults to `7`.

- dodge_width:

  For adjusting the distance between grouped objects. Defaults to `0.8`
  for plots with at least one discrete axis and `0` for plots with two
  continuous axes.

- jitter_width:

  Amount of random noise to be added to the horizontal position of the
  of the data points. This can be useful to deal with overplotting.
  Typical values range between `0` and `1`.

- jitter_height:

  Amount of random noise to be added to the vertical position of the of
  the data points. This can be useful to deal with overplotting. Typical
  values range between `0` and `1`.

- preserve:

  Should dodging preserve the `"total"` width of all elements at a
  position, or the width of a `"single"` element?

- background:

  Whether to include semitransparent background box behind the labels to
  improve legibility. Defaults to `FALSE`.

- background_color:

  Hex color of the background box. Defaults to `"#FFFFFF"` for white.

- background_alpha:

  Opacity of the background box. Defaults to `0.6`.

- label_position:

  Position of the label in relation to the data point. Can be one of
  `c("below", "above", "left", "right", "center")`.

- ...:

  Arguments passed on to the `geom` function.

- segment.size:

  Thickness of the line connecting the label with the data point.
  Defaults to `0.2`.

- box.padding:

  Amount of padding around bounding box, as unit or number. Defaults to
  0.25. (Default unit is lines, but other units can be specified by
  passing `unit(x, "units")`).

- max.overlaps:

  Exclude text labels when they overlap too many other things. For each
  text label, we count how many other text labels or other data points
  it overlaps, and exclude the text label if it has too many overlaps.
  Defaults to 10.

## Value

A `tidyplot` object.

## Details

- `add_data_labels_repel()` uses
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).
  Check there and in [ggrepel
  examples](https://ggrepel.slowkow.com/articles/examples) for
  additional arguments.

- `add_data_labels()` and `add_data_labels_repel()` support data
  subsetting. See [Advanced
  plotting](https://jbengler.github.io/tidyplots/articles/Advanced-plotting.html#data-subsetting).

## Examples

``` r
# Create plot and increase padding to make more space for labels
p <-
  animals |>
  dplyr::slice_head(n = 5) |>
  tidyplot(x = weight, y = speed) |>
  theme_ggplot2() |>
  add_data_points() |>
  adjust_padding(all = 0.3)

# Default label position is `below` the data point
p |> add_data_labels(label = animal)


# Alternative label positions
p |> add_data_labels(label = animal, label_position = "above")


p |> add_data_labels(label = animal, label_position = "right")


p |> add_data_labels(label = animal, label_position = "left")


# Include white background box
p |> add_data_labels(label = animal, background = TRUE)


p |> add_data_labels(label = animal, background = TRUE,
  background_color = "pink")


# Black labels
p |> add_data_labels(label = animal, color = "black")


# Use repelling data labels
p |> add_data_labels_repel(label = animal, color = "black")


p |> add_data_labels_repel(label = animal, color = "black",
  background = TRUE)


p |> add_data_labels_repel(label = animal, color = "black",
  background = TRUE, min.segment.length = 0)

```
