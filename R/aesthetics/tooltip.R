tippy_header <- function(label, tooltip) {
  htmltools::tags$span(
    title = tooltip,
    style = "border-bottom: 1px dotted; cursor: help;",
    label
  )
}
