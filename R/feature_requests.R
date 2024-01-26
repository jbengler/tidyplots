
# it would be nice to have a Graphpad Prism-like x-jitter
# however, availbale sulutions are still not quite there
# https://github.com/eclarke/ggbeeswarm

# also: Does ggbeeswarm work with ggrepel?

# library(tidyverse)
# library(tidyplots)
#
# p <-
#   animals %>%
#   tidyplot(family, size, color = family) %>%
#   add_mean_bar(alpha = 0.3) %>%
#   add_error() %>%
#   adjust_variable(family, sort_by = size)
#
# # default random jitter
# p + ggplot2::geom_point(size = 1)
# p + ggplot2::geom_jitter(width = 0.2, size = 1)
#
# # preserves y values but distribution looks skewed
# p + ggplot2::geom_point(size = 1)
# p + ggbeeswarm::geom_beeswarm(cex = 3, corral = "wrap", corral.width = 0.5, size = 1)
#
# # points are centered, but y values are altered
# p + ggplot2::geom_point(size = 1)
# p + ggbeeswarm::geom_beeswarm(cex = 3.5, method = "center", corral = "wrap", corral.width = 0.5, size = 1)
#
# p + ggplot2::geom_point(size = 1)
# p + ggbeeswarm::geom_quasirandom(size = 1, method = "quasirandom", width = 0.2)
#
# p + ggplot2::geom_point(size = 1)
# p + ggbeeswarm::geom_quasirandom(size = 1, method = "tukey", width = 0.2)
#
#
