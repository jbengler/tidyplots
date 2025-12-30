
facet_wrap_ext <- function(facets, nrow = NULL, ncol = NULL, scales = "free",
                           space = "fixed", shrink = TRUE, labeller = "label_value",
                           as.table = TRUE, switch = deprecated(), drop = TRUE,
                           dir = "h", strip.position = 'top', axes = "all",
                           axis.labels = "all", axis.titles = "all") {
  facet <- ggplot2::facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales,
                               space = space, shrink = shrink, labeller = labeller,
                               as.table = as.table, switch = switch, drop = drop,
                               dir = dir, strip.position = strip.position, axes = axes,
                               axis.labels = axis.labels)
  params <- facet$params
  params$axis.titles <- rlang::arg_match0(axis.titles, c("all", "margins", "single"))
  ggplot2::ggproto(NULL, FacetWrapExt, shrink = shrink, params = params)
}

FacetWrapExt <-
  ggplot2::ggproto("FacetWrapExt",
    ggplot2::FacetWrap,
    draw_labels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, params) {
      draw_axis_titles(panels = panels, labels = labels, axis.titles = self$params$axis.titles)
      }
  )
