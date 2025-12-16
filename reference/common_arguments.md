# Common arguments

Common arguments

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

- shape:

  An `integer` between `0` and `24`, representing the shape of the plot
  symbol.

  ![](figures/unnamed-chunk-1-1.png)

- size:

  A `number` representing the size of the plot symbol. Typical values
  range between `1` and `3`.

- width:

  Horizontal width of the plotted object (bar, error bar, boxplot,
  violin plot, etc). Typical values range between `0` and `1`.

- linewidth:

  Thickness of the line in points (pt). Typical values range between
  `0.25` and `1`.

- ...:

  Arguments passed on to the `geom` function.

- alpha:

  A `number` between `0` and `1` for the opacity of an object. A value
  of `0` is completely transparent, `1` is completely opaque.

- color:

  A hex color for the stroke color. For example, `"#FFFFFF"` for white.

- fill:

  A hex color for the fill color. For example, `"#FFFFFF"` for white.

- saturation:

  A `number` between `0` and `1` for the color saturation of an object.
  A value of `0` is completely desaturated (white), `1` is the original
  color.

- group:

  Variable in the dataset to be used for grouping.

- reverse:

  Whether the order should be reversed or not. Defaults to `FALSE`,
  meaning not reversed.

- .reverse:

  Whether the order should be reversed or not. Defaults to `FALSE`,
  meaning not reversed.

- scale_cut:

  Scale cut function to be applied. See
  [`scales::cut_short_scale()`](https://scales.r-lib.org/reference/number.html)
  and friends.

- fontsize:

  Font size in points. Defaults to `7`.

- replace_na:

  Whether to replace `count = NA` with `count = 0`.

- force_continuous:

  Whether to force the axis to be continuous. Defaults to `FALSE`.

- jitter_width:

  Amount of random noise to be added to the horizontal position of the
  of the data points. This can be useful to deal with overplotting.
  Typical values range between `0` and `1`.

- jitter_height:

  Amount of random noise to be added to the vertical position of the of
  the data points. This can be useful to deal with overplotting. Typical
  values range between `0` and `1`.

## Value

A `tidyplot` object.
