
library(tidyverse)

# investigate saturation
# alpha only works on fill for box, bar and violin!
# is "saturation" really needed?

study %>%
  ggplot(aes(treatment, score, color = treatment, fill = treatment)) +
  geom_boxplot(alpha = 0.2)

study %>%
  ggplot(aes(treatment, score, color = treatment, fill = treatment)) +
  geom_col(alpha = 0.2)

study %>%
  ggplot(aes(treatment, score, color = treatment, fill = treatment)) +
  geom_violin(alpha = 0.2)


# saturation overwrites fill_scale with pale colors.
# coord_grid is not visible through geoms

study %>%
  tidyplot(treatment, score, color = treatment) %>%
  add_mean_bar(alpha = 0.4) %>%
  theme_ggplot2()

study %>%
  tidyplot(treatment, score, color = treatment) %>%
  add(geom_col(alpha = 0.4, color = NA, width = 0.6)) %>%
  theme_ggplot2()

study %>%
  tidyplot(treatment, score, color = treatment) %>%
  add(geom_col(alpha = 0.4, color = NA, width = 0.6)) %>%
  add_error() %>%
  add_points("confetti", jitter_width = 0.2)

study %>%
  tidyplot(treatment, score, color = treatment) %>%
  add_mean_bar(alpha = 0.2) %>%
  add_error() %>%
  add_points("confetti", jitter_width = 0.2)

mapping <- ggplot2::aes(color = dose, fill = ggplot2::after_scale(apply_saturation(colour, saturation)))


# conclusion: keep saturation

##
# try to use after_scale to get lighter bars
# not possible for bars with color = NA

library(tidyverse)
# this works
mpg %>%
  mutate(cyl = factor(cyl)) %>%
  tidyplot(cty, displ, colour = cyl) %>%
  add(geom_col(aes(fill = after_scale(apply_saturation(colour, 0.3)))))

# BUT:
# self reference of fill to fill -> grey fill
mpg %>%
  mutate(cyl = factor(cyl)) %>%
  tidyplot(cty, displ, colour = cyl) %>%
  add(geom_col(aes(fill = after_scale(apply_saturation(fill, 0.3)))))

# fixed color overrides colour, which now can not be used in after_scale() any more
mpg %>%
  mutate(cyl = factor(cyl)) %>%
  tidyplot(cty, displ, colour = cyl) %>%
  add(geom_col(aes(fill = after_scale(apply_saturation(colour, 0.3))), color = "#FF00FF"))

# NA color kills colour, which now can not be used in after_scale() any more
mpg %>%
  mutate(cyl = factor(cyl)) %>%
  tidyplot(cty, displ, colour = cyl) %>%
  add(geom_col(aes(fill = after_scale(apply_saturation(colour, 0.3))), color = NA))


# testing lines

study %>%
  tidyplot(group, score, color = dose) %>%
  add_line() %>%
  add_points()

# order in data set defines how geom_line connects points
study %>%
  dplyr::arrange(score) %>%
  tidyplot(group, score, color = dose) %>%
  add_line() %>%
  add_points()

study %>%
  tidyplot(group, score, color = dose, dodge_width = 0, group = dose) %>%
  add_area(alpha = 0.1) %>%
  add_points()

study %>%
  tidyplot(group, score, color = dose, group = participant) %>%
  add_area(alpha = 0.1) %>%
  add_points()

study %>%
  tidyplot(group, score, color = dose, group = participant, dodge_width = 0) %>%
  add_area(alpha = 0.1) %>%
  add_points()

study %>%
  tidyplot(group, score, color = dose) %>%
  add_areastack_absolute(alpha = 0.1)

# stat_count and stat_sum are ignoring categories with no data
# this is fixed

vars <- c("number_of_legs", "family")

df <-
  animals %>%
  dplyr::summarize(count = dplyr::n(), .by = all_of(vars)) %>%
  tidyr::complete(.data[[vars[1]]], .data[[vars[2]]], fill = list(count = 0))

animals %>%
  tidyplot(number_of_legs, color = family) %>%
  add_areastack_absolute()

df %>%
  tidyplot(number_of_legs, count, color = family) %>%
  add_areastack_absolute()

animals %>%
  tidyplot(number_of_legs, color = family) %>%
  add_areastack_relative()

df %>%
  tidyplot(number_of_legs, count, color = family) %>%
  add_areastack_relative()

# function calls as aesthetics:

gg <-
  study %>%
  ggplot(aes(treatment, cumsum(score))) +
  geom_point()

gg$data
gg$mapping

# meta programming

# https://trinkerrstuff.wordpress.com/2014/08/19/hijacking-r-functions-changing-default-arguments-3/
# https://coolbutuseless.github.io/2018/04/11/changing-the-default-arguments-to-a-function/

# ff with if condition

ff <- function(fun, alert = FALSE) {
  function(x = c(0.5, 5, 10), y = NULL, z = NA) {
    if(alert) message("alert = TRUE")
    fun(x)
  }
}

f_mean_silent <- ff(fun = mean, alert = FALSE)
f_mean_loud <- ff(fun = mean, alert = TRUE)
f_mean_silent()
f_mean_silent(c(5,200))
f_mean_loud()
f_mean_loud(c(5,200))

# function factories
# mean, median, sum, count(=length?), (min, max)

ff <- function(fun) {
  function(x = c(0.5, 5, 10), y = NULL, z = NA) {
    fun(x)
  }
}

f_mean <- ff(mean)
f_mean()
f_mean(c(5,200))

# alternative
# easier to document in roxygen
# fun is exposed for hijacking by the user

gf <- function(x = c(0.5, 5, 10), y = NULL, z = NA, fun = NULL) {
    fun(x)
}

f_mean2 <- function(x = c(0.5, 5, 10), y = NULL, z = NA, fun = mean) {
  gf(x = x, y = y, z = z, fun = fun)
}

f_mean2()
f_mean2(c(5,200))

# add_pie_labels()
# add_barstack_absolute_labels()

spendings %>%
  tidyplot(y = amount, color = category) %>%
  add_pie() %>%
  add(geom_text(aes(x = 3, label = amount), color = "black", position = "stack"))

spendings %>%
  tidyplot(y = amount, color = category) %>%
  add_barstack_absolute() %>%
  add(geom_text(aes(x = 1.5, label = amount), color = "black", position = "stack"))


# position_dodge2(width = dodge_width, preserve = "single")
# all bars have the same width
# BUT:
# dodge_width is ignored
# interpretation of bar width is different between dodge and dodge2
# violins a crippled

# for the sake of consistency I will stick with position_dodge() as default

p <-
  study %>%
  dplyr::filter(!treatment == "A") %>%
  tidyplot(dose, score, color = group)

p %>% add_box() %>% add_points()
p %>% add_violin() %>% add_points() # preserve = "single" destroys violins, therefore not implemented here
p %>% add_error() %>% add_points()
p %>% add_mean_dash() %>% add_points()
p %>% add_mean_bar() %>% add_points()

p <- study %>%
  dplyr::filter(!treatment == "A") %>%
  tidyplot(dose, score, color = group)

p %>% add(geom_boxplot(position = position_dodge(width = 0.2)))
p %>% add(geom_boxplot(position = position_dodge2(width = 0.2, preserve = "single")))

p %>% add(geom_violin(scale = "width", position = position_dodge(width = 0.2)))
p %>% add(geom_violin(scale = "width", position = position_dodge(width = 0.2, preserve = "single")))

p %>% add(geom_col(position = position_dodge(width = 0.2)))
p %>% add(geom_col(position = position_dodge2(width = 0.2, preserve = "single")))

p %>% add(stat_summary(fun = mean, width = 0.8, geom = "bar", color = NA, position = position_dodge(width = 0.2)))
p %>% add(stat_summary(fun = mean, width = 0.8, geom = "bar", color = NA, position = position_dodge2(width = 0.2, preserve = "single")))
p %>% add(stat_summary(fun = mean, width = 0.8, geom = "bar", color = NA, position = position_dodge(width = 0.9)))

# grouped data
# with facets

library(tidyverse)
study %>%
  ggplot(aes(x = group, y = score, group = participant, color = dose)) +
  geom_point() +
  geom_line() +
  facet_wrap(facets = vars(dose))

# testing heatmaps

library(tidyverse)

gene_expression %>%
  dplyr::mutate(row_zscore = (expression - mean(expression)) / sd(expression), .by = external_gene_name) %>%
  tidyplot(x = sample, y = external_gene_name, color = row_zscore) %>%
  add_heatmap() %>%
  adjust_x_axis(rotate_labels = 90) %>%
  adjust_labels(external_gene_name, sort_by = -dplyr::desc(direction)) %>%
  adjust_labels(direction, sort_by = dplyr::desc(direction)) %>%
  adjust_colors(c("blue", "white", "red"), as_palette = TRUE) %>%
  adjust_size(height = 90)

h1 <-
  gene_expression %>%
  dplyr::mutate(row_zscore = (expression - mean(expression)) / sd(expression), .by = external_gene_name) %>%
  dplyr::mutate(replicate = stringr::str_sub(sample, -1)) %>%
  tidyplot(x = replicate, y = external_gene_name, color = row_zscore) %>%
  add_heatmap() %>%
  adjust_x_axis(rotate_labels = 90) %>%
  adjust_labels(external_gene_name, sort_by = -dplyr::desc(direction)) %>%
  adjust_labels(direction, sort_by = dplyr::desc(direction)) %>%
  adjust_colors(c("blue", "white", "red"), as_palette = TRUE) %>%
  adjust_size(height = 90)

h1
h1 + ggplot2::facet_grid(cols = dplyr::vars(group), rows = dplyr::vars(direction), scales = "free_y")
h1 + ggplot2::facet_grid(cols = dplyr::vars(sample_type, condition), rows = dplyr::vars(direction), scales = "free_y")

h2 <- h1 +
  ggplot2::facet_grid(cols = dplyr::vars(condition), rows = dplyr::vars(direction), scales = "free_y")
h2 %>% split_plot(sample_type, heights = 90)

h3 <- h1 %>%
  adjust_colors(c("blue", "white", "red"), as_palette = TRUE, limits = c(-3, 3)) +
  ggplot2::facet_grid(cols = dplyr::vars(condition), rows = dplyr::vars(direction), scales = "free_y")
h3 %>% split_plot(sample_type, heights = 90)


