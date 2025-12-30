
draw_axis_titles <- function(panels, labels, axis.titles = "all") {
  xlab <- labels$x[[2]]
  ylab <- labels$y[[1]]
  xlab_height <- grid::grobHeight(xlab)
  ylab_width <- grid::grobWidth(ylab)

  if (axis.titles == "single") {
    # xlab bottom
    panel_dim <- ggplot2::find_panel(panels)
    panels <- gtable::gtable_add_rows(panels, xlab_height, pos = -1)
    panels <- gtable::gtable_add_grob(panels, xlab, name = "xlab-b",
      l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")

    # ylab left
    panel_dim <- ggplot2::find_panel(panels)
    panels <- gtable::gtable_add_cols(panels, ylab_width, pos = 0)
    panels <- gtable::gtable_add_grob(panels, ylab, name = "ylab-l",
      l = 1, b = panel_dim$b, t = panel_dim$t, clip = "off")

    return(panels)
  }

  # xlab bottom
  layout <- get_existing_axes(panels, "axis-b")
  if ((nrow(layout)) > 0) {
    pos <- unique(layout$t) |> sort(decreasing = TRUE)
    if (axis.titles == "margins") pos <- max(pos)
    for (i in 1:length(pos)) {
      panels <- gtable::gtable_add_rows(panels, xlab_height, pos = pos[i])
    }
  }

  layout <- get_existing_axes(panels, "axis-b")
  if ((nrow(layout)) > 0) {
    if (axis.titles == "margins") layout <- layout |> dplyr::filter(t == max(t), .by = l)
    for (i in 1:length(layout$t)) {
      panels <- gtable::gtable_add_grob(panels, xlab, name = "xlab-b",
                                        t = layout$t[i] + 1, l = layout$l[i], clip = "off")
    }
  }

  # ylab left
  layout <- get_existing_axes(panels, "axis-l")
  if ((nrow(layout)) > 0) {
    pos <- unique(layout$l) |> sort(decreasing = TRUE)
    if (axis.titles == "margins") pos <- min(pos)
    for (i in 1:length(pos)) {
      panels <- gtable::gtable_add_cols(panels, ylab_width, pos = pos[i] - 1)
    }
  }

  layout <- get_existing_axes(panels, "axis-l")
  if ((nrow(layout)) > 0) {
    if (axis.titles == "margins") layout <- layout |> dplyr::filter(l == min(l))
    for (i in 1:length(layout$t)) {
      panels <- gtable::gtable_add_grob(panels, ylab, name = "ylab-l",
                                        t = layout$t[i], l = layout$l[i] - 1, clip = "off")
    }
  }

  panels
}

get_existing_axes <- function(panels, type) {
  is_zero <- vapply(
    panels$grobs,
    inherits,
    logical(1),
    what = "zeroGrob"
  )
  panels$layout[!is_zero, , drop = FALSE] |>
    dplyr::filter(stringr::str_detect(name, type))
}
