# tidyplots (development version)

## Breaking changes

* tidyplots no longer depends on ggpubr. `add_test_pvalue()` and `add_test_asterisks()` now compute the tests themselves with base R, using `stats::t.test()` and `stats::wilcox.test()`. Supported methods are `"t_test"`, `"wilcox_test"`, `"tukey_hsd"`, `"games_howell_test"`, and `"dunn_test"`. Tukey's HSD, Games-Howell, and Dunn's test are computed by tidyplots and verified against `stats::TukeyHSD()` and rstatix. The p values of Tukey's HSD and Games-Howell are adjusted by the test itself, so `p.adjust.method` must be `"none"` for them. The methods `"sign_test"` and `"emmeans_test"` are no longer supported and give an error; use `add_test_pvalue_manual()` with statistics computed elsewhere instead. The appearance of the brackets is nearly unchanged.
* `add_test_pvalue()` and `add_test_asterisks()` now also work in horizontal plots with a discrete y axis and a continuous x axis, e.g. `tidyplot(x = score, y = treatment)`. `add_test_pvalue_manual()` and `add_test_asterisks_manual()` detect the orientation the same way, or take it from the new argument `orientation`.
* `add_test_pvalue()` and `add_test_asterisks()` test the values in the data, regardless of axis transformations and axis limits. Values that are tested but not shown in the plot are reported in a warning and in `view_log()`, and the log names the test as reported by R, e.g. "Wilcoxon rank sum test with continuity correction".
* Arguments in `...` of `add_test_pvalue()`, `add_test_asterisks()`, `add_test_pvalue_manual()`, and `add_test_asterisks_manual()` are no longer passed on to ggpubr. Supported are `method.args` and the drawing arguments `tip.length`, `linewidth` (or `size`), `linetype`, `alpha`, `family`, and `fontface`. Other arguments are ignored with a warning.
* `add_ellipse()` requires the MASS package, which is now listed in Suggests. Previously, it was installed as a dependency of ggpubr.

## Improvements

* Statistical tests are more accurate. p values are no longer rounded before they are shown, and adjusted p values are computed from the exact instead of the rounded p values. For example, a Holm-adjusted p value previously shown as 0.0060 is now correctly shown as 0.0065.
* Statistical tests are computed separately for each panel on the final plot data. Comparisons in facets that previously failed silently are now computed, and comparisons that cannot be computed give a warning. Paired tests match values by `paired_by`, not by their order in the data.
* Experimental: tidyplots now keep a log of what tidyplots did to make a figure. The log records the input data (name, hash, dimensions), the software versions, and every step of the pipeline from `tidyplot()` onward, including ggplot2 components added with `add()` (but not with `+`). Nested internal calls are not logged, and the log does not depend on how functions are invoked (pipe, `do.call()`, `Reduce()`, purrr, wrapper functions). Logging never breaks plotting: steps that cannot be recorded are marked as unrecorded. New `view_log()` prints the log. The log is a record, not a guarantee of reproducibility, and its format may still change.
  * Summary layers record the statistic and dispersion they show (e.g. mean and standard error of the mean), data point and label layers record the rows they show and their jitter seed.
  * Normalization, renaming, reordering and sorting of levels, axis transformations, `flip_plot()` and `split_plot()` record their resolved result, e.g. the reference values of a normalization or the final level order.
  * `add_test_pvalue()` and friends record the test specification in standard terms, e.g. "Welch's t-test", together with the underlying base R function and arguments, the alternative, pairing, comparisons, and the p-value adjustment. The test names are verified by tests against base R, and arguments not reflected in the test name are reported. Pre-computed statistics are recorded as not verified.
  * `view_log()` reports the results of all statistical comparisons: groups, n, test statistic, degrees of freedom, exact and adjusted p values, and significance, per panel. Comparisons hidden with `hide.ns` and comparisons that could not be computed are reported as such.
  * `view_log()` also reports the number of rows plotted, rows dropped due to missing values or non-finite values after an axis transformation, and rows per group. With continuous x and y, summary layers compute their statistic per x value, and the range of rows per x value and group is reported as well.
* Experimental: new `save_bundle()` saves a figure together with its data and log into a folder: a self-contained HTML report with the figure, all steps of the log, and the results of statistical tests, the figure as PNG and PDF, the input and plotted data as CSV with a data dictionary that keeps the order of factor levels, the test results, excluded rows, and the log as text. The log now keeps a reference to the input data.
* New `add_note()` and `add_metadata()` record explanations and metadata such as data source, units, and what one row of the data is (`n_unit`, e.g. `"mice"`) in the log.
* New `data_*()` family for changing the plot data within the pipeline. Changes are recorded in the log.
  * `data_exclude()` excludes rows from the plot data with a required `reason`, following the logic of `dplyr::filter_out()`. The exclusion, its reason, and the excluded rows are recorded in the log.
  * `data_normalize_x()`, `data_normalize_y()`, and `data_normalize_color()` normalize continuous values relative to reference rows, e.g. as fold change, baseline subtraction, or scaling between two references.
* `add()` is now a regular function. Unlike `+`, its additions are recorded in the log.
* New `remove_clipping()` for disabling plot area clipping.
* New `add_test_pvalue_manual()` and `add_test_asterisks_manual()` for adding pre-computed statistical comparisons.
* `add_data_points_jitter()`, `add_data_labels()` and `add_data_labels_repel()` now expose the `seed` parameter.

## Bug fixes

* `add_data_labels_repel()` no longer draws a border around labels with ggrepel 0.9.8 or later, where the border is controlled by `linewidth` instead of `label.size`.
* Axis transformations set with `adjust_x_axis(transform = ...)` or `adjust_y_axis(transform = ...)` now persist. Previously, adding layers such as `add_mean_bar()` or adjusting the axis again silently reset the transformation to `"identity"`.
* `sort_x_axis_levels()` and friends now sort by the summary statistic shown in the plot, as recorded in the log, with the most recent summary layer winning. Previously, the statistic was guessed from function names in the call history, so plots with only `add_sem_errorbar()`, `add_sd_errorbar()` or `add_ci95_errorbar()` were sorted by median instead of mean, and calls via `Reduce()` or purrr lost the statistic.
* Calling tidyplots functions via `do.call()` no longer fails.
* `add_test_pvalue()` and `add_test_asterisks()` no longer fail when `method.args` is supplied, e.g. `method.args = list(var.equal = TRUE)`.
* `save_plot()` no longer uses the deprecated `ggplot2::is.ggplot()`, and no longer creates an `Rplots.pdf` file in the working directory when no graphics device is open, e.g. in scripts.
* Color saturation now persist multiple `adjust_colors()` calls (#171).
* `adjust_legend_title()` now works across all mapped aesthetics (#163).

# tidyplots 0.4.0

## Breaking changes

* `split_plot()` is now powered by the ggplot2 faceting functions `ggplot2::facet_wrap()` and `ggplot2::facet_grid()`, 
thereby replacing `patchwork::wrap_plots()`. While this improves consistency and solves glitches in figure legends, 
it will eventually break code that relies on patchwork syntax (#95, #141).
* In `add_violin()`, `trim = TRUE` is now the default. This improves accuracy when plotting small sample sizes and 
aligns the behavior with `ggplot2::geom_violin()` (#151).

## Bug fixes

* Avoid rounding errors of _p_ values in `add_test_pvalue()` (#142).

## Improvements

* `split_plot()` gains the parameters `rows` and `cols` allowing to split the plot by two variables. 
This functionality is powered by `ggplot2::facet_grid()`.
* `split_plot()` gains the parameter `axis.titles` allowing to draw a `"single"` axis title, `"all"` axis titles, or just those on the layout `"margins"`.
* `adjust_size()` gains the parameters `overall_width` and `overall_height` to set the dimensions of the entire multiplot layout generated with `split_plot()`. 
In contrast, the existing `width` and `height` parameters determine the size of each individual plot within the layout.
* `tidyplot()` gains the parameters `paper` and `ink` allowing to choose a color for the background (`paper`) and the 
foreground elements like text and lines (`ink`). This allows for global color customization, such as enabling dark-mode plots.
* `tidyplots_options()` gains the parameters `paper` and `ink` allowing to set these parameters once for all tidyplots in the active R session.
* The `labels` parameter of `adjust_*_axis()` now survives repeated calls changing the same scale (#136).
* Fixed `add_data_points()` does now respect constant `color` when `white_border = TRUE` (#115).
* Prepare for deprecation of `%+%` and `geom_label(label.size = NA)`.

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
