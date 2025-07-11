# tidyplots (development version)

## Improvements

* Fixed `add_data_points()` to respect constant `color` when `white_border = TRUE` (#115)
* Prepare for deprecation of `%+%` and `geom_label(label.size = NA)`

# tidyplots 0.3.1

## Breaking changes
* Removed the parameters `widths` and `heights` from `split_plot()`. Use 
`adjust_size()` before `split_plot()` instead.
* Removed the function `format_number()`, which is available from the scales packages.
* Functions to rename, reorder, sort and reverse the levels of variables mapped to x, y, and color 
now have the suffix `_levels()` instead of `_lables()`. For example, `rename_x_axis_levels()` (#113).

## Improvements

* New parameter `paired_by` in `add_test_pvalue()` and `add_test_asterisks()` 
enables paired comparisons.
* New parameter `comparisons` in `add_test_pvalue()` and `add_test_asterisks()` 
enables selected comparisons (#82).
* New parameter `my_style` in `tidyplot()` for providing a styling function (#85).
* Support for global `tidyplots_options()` affecting all tidyplots in the current session. 
Supported options include `width`, `height`, `unit`, `dodge_width`, and `my_style` (#84, #85).
* `add_annotation_text()` now supports colored text (#86).
* Support for axis `limits` when the axis is of type `date`, `time`, or `datetime` (#97, #99).
* A default jitter seed in `add_data_points_jitter()`, `add_data_labels()` and `add_data_labels_repel()` 
now facilitates the alignment of jittered labels and points (#104).
* Fixed `add_histogram()` now respects the `color` parameter (#106).
* New dataset `pca` containing a principle component analysis.
* New `add_ellipse()` function (#52).

# tidyplots 0.2.2

This is a patch release mainly focusing on preparing tidyplots for the upcoming release of ggplot2 3.6.0.

## Breaking changes

* Hard deprecation of `as_tidyplot()`. Converting a ggplot to a tidyplot probably never was a good idea.

## Improvements

* Support ordered factors provided to color (#75)
* Prepare tidyplots for upcoming ggplot2 3.6.0 release (#60)
* Switch from the magrittr pipe `%>%` to the base R pipe `|>` in both the documentation and code (#55, #56)
* More meaningful error for invalid plotmath expressions.
* Update documentation (@mthulin, #62)

# tidyplots 0.2.1

## Breaking changes

* The `energy` dataset has been updated to contain the correct energy values in TWh. The variable `power` has been renamed to `energy`. This change will affect all code that uses the `energy` dataset.

## Bug fixes

* The `limits` parameter of `adjust_x_axis()` and `adjust_y_axis()` had no effect when combined with `add_count_*()` (#41).

## Improvements

* New color scheme `colors_discrete_rainbow` (@electrolars, #35).
* `save_plot()` gains `view_plot` argument to control whether to view the plot on screen after saving (#40).
* New `add_annotation_text()`, `add_annotation_rectangle()`, `add_annotation_segment()` for including annotation at specific coordinates (#38).

# tidyplots 0.2.0

## Bug fixes

* The standard deviation (SD) in `add_sd_errorbar()` and `add_sd_ribbon()` is now shown as
one time SD, not 2 times SD as before. Thanks to @awata25 for spotting this (#25).

## Improvements

* Improved documentation (#6).
* The default `dodge_width` is now determined by a heuristic (#13).
* Tidyplots now requires ggplot2 (>= 3.5.0) (#16).
* The minimal themes `theme_minimal_*()` now have axis ticks.
* New color scheme `colors_discrete_alger` suggested by @loukesio (#18).
* New function `adjust_theme_details()` (#23)
* New arguments `fontsize`, `family`, `face`, and `color` in 
`adjust_title()`, `adjust_caption()`, `adjust_x_axis_title()`,  `adjust_y_axis_title()`, 
and `adjust_legend_title()` for more detailed control over fonts (#24).

# tidyplots 0.1.2

* Initial CRAN release
* New S3 class `tidycolor` for color schemes. The print method of `tidycolor` sends a html preview of the color scheme to the RStudio viewer panel.
* New `new_color_scheme()` to create custom color schemes.
* New build-in color schemes using the prefix `colors_discrete_`, `colors_continuous_` and `colors_diverging_`. 
* `adjust_colors()` now also works with too few or too many provided colors.
* New function factory behind `adjust_x_axis()` and `adjust_y_axis()`
* Updated README and documentation.

# tidyplots 0.0.2

* The package is still in early development. Expect user-facing and breaking changes.
* Renaming functions to improve consistency.

# tidyplots 0.0.1

* The package is still in early development. Expect user-facing and breaking changes.
* Initial release.
