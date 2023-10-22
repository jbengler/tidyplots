
library(tidyverse)

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

p <- study %>%
  filter(!treatment == "A") %>%
  tidyplot(dose, score, color = group)

p %>% add(geom_boxplot(position = position_dodge(width = 0.2)))
p %>% add(geom_boxplot(position = position_dodge2(width = 0.2, preserve = "single")))

p %>% add(geom_violin(scale = "width", position = position_dodge(width = 0.2)))
p %>% add(geom_violin(scale = "width", position = position_dodge2(width = 0.2, preserve = "single")))

p %>% add(geom_col(position = position_dodge(width = 0.2)))
p %>% add(geom_col(position = position_dodge2(width = 0.2, preserve = "single")))

p %>% add(stat_summary(fun = mean, width = 0.8, geom = "bar", color = NA, position = position_dodge(width = 0.2)))
p %>% add(stat_summary(fun = mean, width = 0.9, geom = "bar", color = NA, position = position_dodge2(width = 0.2, preserve = "single")))
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


