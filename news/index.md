# Changelog

## tidyplots (development version)

### Breaking changes

- [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md)
  is now powered by the ggplot2 faceting functions
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  thereby replacing
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  While this improves consistency and solves glitches in figure legends,
  it will eventually break code that relies on patchwork syntax
  ([\#95](https://github.com/jbengler/tidyplots/issues/95),
  [\#141](https://github.com/jbengler/tidyplots/issues/141)).

### Bug fixes

- Avoid rounding errors of *p* values in
  [`add_test_pvalue()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  ([\#142](https://github.com/jbengler/tidyplots/issues/142)).

### Improvements

- [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md)
  gains the parameters `rows` and `cols` allowing to split the plot by
  two variables. This functionality is powered by
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
- [`adjust_size()`](https://jbengler.github.io/tidyplots/reference/adjust_size.md)
  gains the parameters `overall_width` and `overall_height` allowing to
  set the overall dimensions of a multiplot layout generated with
  [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md).
- [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md)
  gains the parameters `paper` and `ink` allowing to choose a color for
  the background (`paper`) and the foreground elements like text and
  lines (`ink`). This is useful to generate plots for dark mode.
- [`tidyplots_options()`](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md)
  gains the parameters `paper` and `ink` allowing to set these
  parameters once for all tidyplots in the active R session.
- The `labels` parameter of `adjust_*_axis()` now survives repeated
  calls changing the same scale
  ([\#136](https://github.com/jbengler/tidyplots/issues/136)).
- Fixed
  [`add_data_points()`](https://jbengler.github.io/tidyplots/reference/add_data_points.md)
  to respect constant `color` when `white_border = TRUE`
  ([\#115](https://github.com/jbengler/tidyplots/issues/115)).
- Prepare for deprecation of `%+%` and `geom_label(label.size = NA)`.

## tidyplots 0.3.1

CRAN release: 2025-07-02

### Breaking changes

- Removed the parameters `widths` and `heights` from
  [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md).
  Use
  [`adjust_size()`](https://jbengler.github.io/tidyplots/reference/adjust_size.md)
  before
  [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md)
  instead.
- Removed the function `format_number()`, which is available from the
  scales packages.
- Functions to rename, reorder, sort and reverse the levels of variables
  mapped to x, y, and color now have the suffix `_levels()` instead of
  `_lables()`. For example,
  [`rename_x_axis_levels()`](https://jbengler.github.io/tidyplots/reference/rename_x_axis_levels.md)
  ([\#113](https://github.com/jbengler/tidyplots/issues/113)).

### Improvements

- New parameter `paired_by` in
  [`add_test_pvalue()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  and
  [`add_test_asterisks()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  enables paired comparisons.
- New parameter `comparisons` in
  [`add_test_pvalue()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  and
  [`add_test_asterisks()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  enables selected comparisons
  ([\#82](https://github.com/jbengler/tidyplots/issues/82)).
- New parameter `my_style` in
  [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md)
  for providing a styling function
  ([\#85](https://github.com/jbengler/tidyplots/issues/85)).
- Support for global
  [`tidyplots_options()`](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md)
  affecting all tidyplots in the current session. Supported options
  include `width`, `height`, `unit`, `dodge_width`, and `my_style`
  ([\#84](https://github.com/jbengler/tidyplots/issues/84),
  [\#85](https://github.com/jbengler/tidyplots/issues/85)).
- [`add_annotation_text()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md)
  now supports colored text
  ([\#86](https://github.com/jbengler/tidyplots/issues/86)).
- Support for axis `limits` when the axis is of type `date`, `time`, or
  `datetime` ([\#97](https://github.com/jbengler/tidyplots/issues/97),
  [\#99](https://github.com/jbengler/tidyplots/issues/99)).
- A default jitter seed in
  [`add_data_points_jitter()`](https://jbengler.github.io/tidyplots/reference/add_data_points.md),
  [`add_data_labels()`](https://jbengler.github.io/tidyplots/reference/add_data_labels.md)
  and
  [`add_data_labels_repel()`](https://jbengler.github.io/tidyplots/reference/add_data_labels.md)
  now facilitates the alignment of jittered labels and points
  ([\#104](https://github.com/jbengler/tidyplots/issues/104)).
- Fixed
  [`add_histogram()`](https://jbengler.github.io/tidyplots/reference/add_histogram.md)
  now respects the `color` parameter
  ([\#106](https://github.com/jbengler/tidyplots/issues/106)).
- New dataset `pca` containing a principle component analysis.
- New
  [`add_ellipse()`](https://jbengler.github.io/tidyplots/reference/add_ellipse.md)
  function ([\#52](https://github.com/jbengler/tidyplots/issues/52)).

## tidyplots 0.2.2

CRAN release: 2025-03-07

This is a patch release mainly focusing on preparing tidyplots for the
upcoming release of ggplot2 3.6.0.

### Breaking changes

- Hard deprecation of `as_tidyplot()`. Converting a ggplot to a tidyplot
  probably never was a good idea.

### Improvements

- Support ordered factors provided to color
  ([\#75](https://github.com/jbengler/tidyplots/issues/75))
- Prepare tidyplots for upcoming ggplot2 3.6.0 release
  ([\#60](https://github.com/jbengler/tidyplots/issues/60))
- Switch from the magrittr pipe `%>%` to the base R pipe `|>` in both
  the documentation and code
  ([\#55](https://github.com/jbengler/tidyplots/issues/55),
  [\#56](https://github.com/jbengler/tidyplots/issues/56))
- More meaningful error for invalid plotmath expressions.
- Update documentation ([@mthulin](https://github.com/mthulin),
  [\#62](https://github.com/jbengler/tidyplots/issues/62))

## tidyplots 0.2.1

CRAN release: 2025-01-19

### Breaking changes

- The `energy` dataset has been updated to contain the correct energy
  values in TWh. The variable `power` has been renamed to `energy`. This
  change will affect all code that uses the `energy` dataset.

### Bug fixes

- The `limits` parameter of
  [`adjust_x_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
  and
  [`adjust_y_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
  had no effect when combined with `add_count_*()`
  ([\#41](https://github.com/jbengler/tidyplots/issues/41)).

### Improvements

- New color scheme `colors_discrete_rainbow`
  ([@electrolars](https://github.com/electrolars),
  [\#35](https://github.com/jbengler/tidyplots/issues/35)).
- [`save_plot()`](https://jbengler.github.io/tidyplots/reference/save_plot.md)
  gains `view_plot` argument to control whether to view the plot on
  screen after saving
  ([\#40](https://github.com/jbengler/tidyplots/issues/40)).
- New
  [`add_annotation_text()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md),
  [`add_annotation_rectangle()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md),
  `add_annotation_segment()` for including annotation at specific
  coordinates ([\#38](https://github.com/jbengler/tidyplots/issues/38)).

## tidyplots 0.2.0

CRAN release: 2024-12-16

### Bug fixes

- The standard deviation (SD) in
  [`add_sd_errorbar()`](https://jbengler.github.io/tidyplots/reference/add_sem_errorbar.md)
  and
  [`add_sd_ribbon()`](https://jbengler.github.io/tidyplots/reference/add_sem_ribbon.md)
  is now shown as one time SD, not 2 times SD as before. Thanks to
  [@awata25](https://github.com/awata25) for spotting this
  ([\#25](https://github.com/jbengler/tidyplots/issues/25)).

### Improvements

- Improved documentation
  ([\#6](https://github.com/jbengler/tidyplots/issues/6)).
- The default `dodge_width` is now determined by a heuristic
  ([\#13](https://github.com/jbengler/tidyplots/issues/13)).
- Tidyplots now requires ggplot2 (\>= 3.5.0)
  ([\#16](https://github.com/jbengler/tidyplots/issues/16)).
- The minimal themes `theme_minimal_*()` now have axis ticks.
- New color scheme `colors_discrete_alger` suggested by
  [@loukesio](https://github.com/loukesio)
  ([\#18](https://github.com/jbengler/tidyplots/issues/18)).
- New function
  [`adjust_theme_details()`](https://jbengler.github.io/tidyplots/reference/adjust_theme_details.md)
  ([\#23](https://github.com/jbengler/tidyplots/issues/23))
- New arguments `fontsize`, `family`, `face`, and `color` in
  [`adjust_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md),
  [`adjust_caption()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md),
  [`adjust_x_axis_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md),
  [`adjust_y_axis_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md),
  and
  [`adjust_legend_title()`](https://jbengler.github.io/tidyplots/reference/adjust_legend_title.md)
  for more detailed control over fonts
  ([\#24](https://github.com/jbengler/tidyplots/issues/24)).

## tidyplots 0.1.2

CRAN release: 2024-11-08

- Initial CRAN release
- New S3 class `tidycolor` for color schemes. The print method of
  `tidycolor` sends a html preview of the color scheme to the RStudio
  viewer panel.
- New
  [`new_color_scheme()`](https://jbengler.github.io/tidyplots/reference/new_color_scheme.md)
  to create custom color schemes.
- New build-in color schemes using the prefix `colors_discrete_`,
  `colors_continuous_` and `colors_diverging_`.
- [`adjust_colors()`](https://jbengler.github.io/tidyplots/reference/adjust_colors.md)
  now also works with too few or too many provided colors.
- New function factory behind
  [`adjust_x_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
  and
  [`adjust_y_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
- Updated README and documentation.

## tidyplots 0.0.2

- The package is still in early development. Expect user-facing and
  breaking changes.
- Renaming functions to improve consistency.

## tidyplots 0.0.1

- The package is still in early development. Expect user-facing and
  breaking changes.
- Initial release.
