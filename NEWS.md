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
