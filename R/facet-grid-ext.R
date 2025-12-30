
facet_grid_ext <- function(rows = NULL, cols = NULL, scales = "fixed",
                           space = "fixed", shrink = TRUE,
                           labeller = "label_value", as.table = TRUE,
                           switch = "y", drop = TRUE, margins = FALSE,
                           axes = "all", axis.labels = "all", axis.titles = "all") {
  facet <- ggplot2::facet_grid(rows = rows, cols = cols, scales = scales,
                               space = space, shrink = shrink,
                               labeller = labeller, as.table = as.table,
                               switch = switch, drop = drop, margins = margins,
                               axes = axes, axis.labels = axis.labels)
  params <- facet$params
  params$axis.titles <- rlang::arg_match0(axis.titles, c("all", "margins", "single"))
  ggplot2::ggproto(NULL, FacetGridExt, shrink = shrink, params = params)
}

FacetGridExt <-
  ggplot2::ggproto("FacetGridExt",
    ggplot2::FacetGrid,
    draw_labels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, params) {
      draw_axis_titles(panels = panels, labels = labels, axis.titles = self$params$axis.titles)
      }
  )
