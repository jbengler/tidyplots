# tidyplots (development version)

* New S3 class `tidycolor` for color schemes. The print method of `tidycolor` sends a html preview of the color scheme to the RStudio viewer panel.
* New `new_color_scheme()` to create custom color schemes.
* New build-in color schemes using the prefix `colors_discrete_`, `colors_continuous_` and `colors_diverging_`. 
* `adjust_colors()` now also works with too few or too many provided colors.
* New function factory behind `adjust_x_axis()` and `adjust_y_axis()`

# tidyplots 0.0.2

* Renaming functions to improve consistency.
* The package is still in early development. Expect user-facing and breaking changes.

# tidyplots 0.0.1

* Initial release. tidyplots streamlines the creation of publication-ready plots for scientific papers, making it incredibly easy to add and refine plot elements. It allows precise control over composition, style, and absolute sizes, while its utilization of the pipe `%>%` simplifies the construction of advanced plotting pipelines.
* The package is still in early development. Expect user-facing and breaking changes.
