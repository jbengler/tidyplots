# Package index

To create a plot, start with the
[`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md)
function. Build the plot by adding, removing, and adjusting plot
components. The
[`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md)
function must be called last in this sequence and can only be followed
by
[`save_plot()`](https://jbengler.github.io/tidyplots/reference/save_plot.md).

## Create

- [`tidyplot()`](https://jbengler.github.io/tidyplots/reference/tidyplot.md)
  : Create a new tidyplot

## Add

Add plot components to represent data points, central tendency measures,
dispersion and more.

### Data points & amounts

- [`add_data_points()`](https://jbengler.github.io/tidyplots/reference/add_data_points.md)
  [`add_data_points_jitter()`](https://jbengler.github.io/tidyplots/reference/add_data_points.md)
  [`add_data_points_beeswarm()`](https://jbengler.github.io/tidyplots/reference/add_data_points.md)
  : Add data points
- [`add_count_bar()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  [`add_count_dash()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  [`add_count_dot()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  [`add_count_value()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  [`add_count_line()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  [`add_count_area()`](https://jbengler.github.io/tidyplots/reference/add_count_bar.md)
  : Add count
- [`add_sum_bar()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  [`add_sum_dash()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  [`add_sum_dot()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  [`add_sum_value()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  [`add_sum_line()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  [`add_sum_area()`](https://jbengler.github.io/tidyplots/reference/add_sum_bar.md)
  : Add sum
- [`add_heatmap()`](https://jbengler.github.io/tidyplots/reference/add_heatmap.md)
  : Add heatmap
- [`add_line()`](https://jbengler.github.io/tidyplots/reference/add_line.md)
  [`add_area()`](https://jbengler.github.io/tidyplots/reference/add_line.md)
  : Add line or area

### Central tendency

- [`add_mean_bar()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  [`add_mean_dash()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  [`add_mean_dot()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  [`add_mean_value()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  [`add_mean_line()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  [`add_mean_area()`](https://jbengler.github.io/tidyplots/reference/add_mean_bar.md)
  : Add mean
- [`add_median_bar()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  [`add_median_dash()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  [`add_median_dot()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  [`add_median_value()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  [`add_median_line()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  [`add_median_area()`](https://jbengler.github.io/tidyplots/reference/add_median_bar.md)
  : Add median
- [`add_curve_fit()`](https://jbengler.github.io/tidyplots/reference/add_curve_fit.md)
  : Add curve fit

### Distribution & uncertainty

- [`add_histogram()`](https://jbengler.github.io/tidyplots/reference/add_histogram.md)
  : Add histogram
- [`add_boxplot()`](https://jbengler.github.io/tidyplots/reference/add_boxplot.md)
  : Add boxplot
- [`add_violin()`](https://jbengler.github.io/tidyplots/reference/add_violin.md)
  : Add violin plot
- [`add_sem_errorbar()`](https://jbengler.github.io/tidyplots/reference/add_sem_errorbar.md)
  [`add_range_errorbar()`](https://jbengler.github.io/tidyplots/reference/add_sem_errorbar.md)
  [`add_sd_errorbar()`](https://jbengler.github.io/tidyplots/reference/add_sem_errorbar.md)
  [`add_ci95_errorbar()`](https://jbengler.github.io/tidyplots/reference/add_sem_errorbar.md)
  : Add error bar
- [`add_sem_ribbon()`](https://jbengler.github.io/tidyplots/reference/add_sem_ribbon.md)
  [`add_range_ribbon()`](https://jbengler.github.io/tidyplots/reference/add_sem_ribbon.md)
  [`add_sd_ribbon()`](https://jbengler.github.io/tidyplots/reference/add_sem_ribbon.md)
  [`add_ci95_ribbon()`](https://jbengler.github.io/tidyplots/reference/add_sem_ribbon.md)
  : Add ribbon
- [`add_ellipse()`](https://jbengler.github.io/tidyplots/reference/add_ellipse.md)
  : Add ellipse

### Proportion

- [`add_barstack_absolute()`](https://jbengler.github.io/tidyplots/reference/add_barstack_absolute.md)
  [`add_barstack_relative()`](https://jbengler.github.io/tidyplots/reference/add_barstack_absolute.md)
  : Add bar stack
- [`add_areastack_absolute()`](https://jbengler.github.io/tidyplots/reference/add_areastack_absolute.md)
  [`add_areastack_relative()`](https://jbengler.github.io/tidyplots/reference/add_areastack_absolute.md)
  : Add area stack
- [`add_pie()`](https://jbengler.github.io/tidyplots/reference/add_pie.md)
  [`add_donut()`](https://jbengler.github.io/tidyplots/reference/add_pie.md)
  : Add pie or donut chart

### Statistical testing

- [`add_test_pvalue()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  [`add_test_asterisks()`](https://jbengler.github.io/tidyplots/reference/add_test_pvalue.md)
  : Add statistical test

### Annotation

- [`add_title()`](https://jbengler.github.io/tidyplots/reference/add_title.md)
  [`add_caption()`](https://jbengler.github.io/tidyplots/reference/add_title.md)
  : Add plot title or caption
- [`add_data_labels()`](https://jbengler.github.io/tidyplots/reference/add_data_labels.md)
  [`add_data_labels_repel()`](https://jbengler.github.io/tidyplots/reference/add_data_labels.md)
  : Add data labels
- [`add_reference_lines()`](https://jbengler.github.io/tidyplots/reference/add_reference_lines.md)
  : Add reference lines
- [`add_annotation_text()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md)
  [`add_annotation_rectangle()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md)
  [`add_annotation_line()`](https://jbengler.github.io/tidyplots/reference/add_annotation_text.md)
  : Add annotation

## Remove

Remove plot components.

- [`remove_legend()`](https://jbengler.github.io/tidyplots/reference/remove_legend.md)
  [`remove_legend_title()`](https://jbengler.github.io/tidyplots/reference/remove_legend.md)
  : Remove legend or legend title
- [`remove_padding()`](https://jbengler.github.io/tidyplots/reference/remove_padding.md)
  : Remove plot area padding
- [`remove_title()`](https://jbengler.github.io/tidyplots/reference/remove_title.md)
  [`remove_caption()`](https://jbengler.github.io/tidyplots/reference/remove_title.md)
  : Remove plot title or caption
- [`remove_x_axis()`](https://jbengler.github.io/tidyplots/reference/remove_x_axis.md)
  [`remove_x_axis_line()`](https://jbengler.github.io/tidyplots/reference/remove_x_axis.md)
  [`remove_x_axis_ticks()`](https://jbengler.github.io/tidyplots/reference/remove_x_axis.md)
  [`remove_x_axis_labels()`](https://jbengler.github.io/tidyplots/reference/remove_x_axis.md)
  [`remove_x_axis_title()`](https://jbengler.github.io/tidyplots/reference/remove_x_axis.md)
  : Remove x-axis or parts of it
- [`remove_y_axis()`](https://jbengler.github.io/tidyplots/reference/remove_y_axis.md)
  [`remove_y_axis_line()`](https://jbengler.github.io/tidyplots/reference/remove_y_axis.md)
  [`remove_y_axis_ticks()`](https://jbengler.github.io/tidyplots/reference/remove_y_axis.md)
  [`remove_y_axis_labels()`](https://jbengler.github.io/tidyplots/reference/remove_y_axis.md)
  [`remove_y_axis_title()`](https://jbengler.github.io/tidyplots/reference/remove_y_axis.md)
  : Remove y-axis or parts of it

## Adjust

Adjust plot components, properties, and data levels.

### Components & properties

- [`adjust_colors()`](https://jbengler.github.io/tidyplots/reference/adjust_colors.md)
  : Adjust colors
- [`adjust_font()`](https://jbengler.github.io/tidyplots/reference/adjust_font.md)
  : Adjust font
- [`adjust_legend_title()`](https://jbengler.github.io/tidyplots/reference/adjust_legend_title.md)
  [`adjust_legend_position()`](https://jbengler.github.io/tidyplots/reference/adjust_legend_title.md)
  : Adjust legend
- [`adjust_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md)
  [`adjust_x_axis_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md)
  [`adjust_y_axis_title()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md)
  [`adjust_caption()`](https://jbengler.github.io/tidyplots/reference/adjust_title.md)
  : Adjust titles and caption
- [`adjust_size()`](https://jbengler.github.io/tidyplots/reference/adjust_size.md)
  : Adjust plot area size
- [`adjust_padding()`](https://jbengler.github.io/tidyplots/reference/adjust_padding.md)
  : Adjust plot area padding
- [`adjust_x_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
  [`adjust_y_axis()`](https://jbengler.github.io/tidyplots/reference/adjust_x_axis.md)
  : Adjust axes

### Axis and color levels

- [`rename_x_axis_levels()`](https://jbengler.github.io/tidyplots/reference/rename_x_axis_levels.md)
  [`rename_y_axis_levels()`](https://jbengler.github.io/tidyplots/reference/rename_x_axis_levels.md)
  [`rename_color_levels()`](https://jbengler.github.io/tidyplots/reference/rename_x_axis_levels.md)
  : Rename axis or color levels
- [`reorder_x_axis_levels()`](https://jbengler.github.io/tidyplots/reference/reorder_x_axis_levels.md)
  [`reorder_y_axis_levels()`](https://jbengler.github.io/tidyplots/reference/reorder_x_axis_levels.md)
  [`reorder_color_levels()`](https://jbengler.github.io/tidyplots/reference/reorder_x_axis_levels.md)
  : Reorder axis or color levels
- [`sort_x_axis_levels()`](https://jbengler.github.io/tidyplots/reference/sort_x_axis_levels.md)
  [`sort_y_axis_levels()`](https://jbengler.github.io/tidyplots/reference/sort_x_axis_levels.md)
  [`sort_color_levels()`](https://jbengler.github.io/tidyplots/reference/sort_x_axis_levels.md)
  : Sort axis or color levels
- [`reverse_x_axis_levels()`](https://jbengler.github.io/tidyplots/reference/reverse_x_axis_levels.md)
  [`reverse_y_axis_levels()`](https://jbengler.github.io/tidyplots/reference/reverse_x_axis_levels.md)
  [`reverse_color_levels()`](https://jbengler.github.io/tidyplots/reference/reverse_x_axis_levels.md)
  : Reverse axis or color levels

## Themes

Finetune the look of the plot.

- [`theme_tidyplot()`](https://jbengler.github.io/tidyplots/reference/theme_tidyplot.md)
  [`theme_ggplot2()`](https://jbengler.github.io/tidyplots/reference/theme_tidyplot.md)
  [`theme_minimal_xy()`](https://jbengler.github.io/tidyplots/reference/theme_tidyplot.md)
  [`theme_minimal_x()`](https://jbengler.github.io/tidyplots/reference/theme_tidyplot.md)
  [`theme_minimal_y()`](https://jbengler.github.io/tidyplots/reference/theme_tidyplot.md)
  : Themes
- [`adjust_theme_details()`](https://jbengler.github.io/tidyplots/reference/adjust_theme_details.md)
  : Adjust theme details
- [`tidyplots_options()`](https://jbengler.github.io/tidyplots/reference/tidyplots_options.md)
  : Tidyplots options

## Color schemes

Choose from a wide range of color schemes.

- [`colors_discrete_friendly`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_seaside`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_apple`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_friendly_long`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_okabeito`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_ibm`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_metro`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_candy`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_alger`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  [`colors_discrete_rainbow`](https://jbengler.github.io/tidyplots/reference/colors_discrete_friendly.md)
  : Discrete color schemes
- [`colors_continuous_viridis`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_magma`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_inferno`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_plasma`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_cividis`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_rocket`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_mako`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_turbo`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  [`colors_continuous_bluepinkyellow`](https://jbengler.github.io/tidyplots/reference/colors_continuous_viridis.md)
  : Continuous color schemes
- [`colors_diverging_blue2red`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  [`colors_diverging_blue2brown`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  [`colors_diverging_BuRd`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  [`colors_diverging_BuYlRd`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  [`colors_diverging_spectral`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  [`colors_diverging_icefire`](https://jbengler.github.io/tidyplots/reference/colors_diverging_blue2red.md)
  : Diverging color schemes
- [`new_color_scheme()`](https://jbengler.github.io/tidyplots/reference/new_color_scheme.md)
  : New color scheme

## Split

Split the plot into a multi plot layout, eventually across multiple
pages.

- [`split_plot()`](https://jbengler.github.io/tidyplots/reference/split_plot.md)
  : Split plot into multiple subplots

## Output

Output the final plot or intermediate stages to the screen or to a file.

- [`view_plot()`](https://jbengler.github.io/tidyplots/reference/view_plot.md)
  : View plot on screen
- [`save_plot()`](https://jbengler.github.io/tidyplots/reference/save_plot.md)
  : Save plots to file

## Helpers

- [`all_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`filter_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`max_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`min_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`first_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`last_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  [`sample_rows()`](https://jbengler.github.io/tidyplots/reference/all_rows.md)
  : Subset data rows
- [`add()`](https://jbengler.github.io/tidyplots/reference/add.md) : Add
  ggplot2 code to a tidyplot
- [`flip_plot()`](https://jbengler.github.io/tidyplots/reference/flip_plot.md)
  **\[superseded\]** : Flip x and y-axis
- [`format_p_value()`](https://jbengler.github.io/tidyplots/reference/format_p_value.md)
  : Format p values

## Data

- [`animals`](https://jbengler.github.io/tidyplots/reference/animals.md)
  : Animals data
- [`climate`](https://jbengler.github.io/tidyplots/reference/climate.md)
  : Climate data
- [`dinosaurs`](https://jbengler.github.io/tidyplots/reference/dinosaurs.md)
  : Dinosaurs data
- [`distributions`](https://jbengler.github.io/tidyplots/reference/distributions.md)
  : Distributions data
- [`energy`](https://jbengler.github.io/tidyplots/reference/energy.md) :
  Energy data
- [`energy_week`](https://jbengler.github.io/tidyplots/reference/energy_week.md)
  : Energy week data
- [`eu_countries`](https://jbengler.github.io/tidyplots/reference/eu_countries.md)
  : EU countries data
- [`gene_expression`](https://jbengler.github.io/tidyplots/reference/gene_expression.md)
  : RNA-Seq expression data
- [`spendings`](https://jbengler.github.io/tidyplots/reference/spendings.md)
  : Spending data
- [`study`](https://jbengler.github.io/tidyplots/reference/study.md) :
  Study data
- [`time_course`](https://jbengler.github.io/tidyplots/reference/time_course.md)
  : Time course data
- [`pca`](https://jbengler.github.io/tidyplots/reference/pca.md) :
  Principle component analysis data
