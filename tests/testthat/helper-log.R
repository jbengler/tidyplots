# Helpers for the log tests

log_funs <- function(plot) {
  vapply(plot$tidyplot$log$steps, `[[`, character(1), "fun")
}

log_kinds_of <- function(plot) {
  vapply(plot$tidyplot$log$steps, `[[`, character(1), "kind")
}
