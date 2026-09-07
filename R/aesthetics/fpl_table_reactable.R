library(reactable)

fpl_table <- function(data, ...) {
  reactable(
    data,
    pagination  = TRUE,
    pageSizeOptions = c(20, 50),
    resizable = TRUE,
    striped     = TRUE,
    highlight   = TRUE,
    bordered    = TRUE,
    compact     = TRUE,
    theme = reactableTheme(
      style = list(
        fontFamily = "'Montserrat', sans-serif",
        fontSize   = "8px",
        color      = "#F2F2F2",
        background = "#303030"
      ),
      headerStyle = list(
        background  = "#1A1A1A",
        color       = "#F2F2F2",
        borderColor = "#3A3A3A"
      ),
      groupHeaderStyle = list(
        fontFamily = "Montserrat, sans-serif",
        fontSize   = "12px",
        textAlign  = "center",
        padding    = "4px 6px"
      ),
      borderColor          = "#3A3A3A",
      stripedColor         = "#454545",
      highlightColor       = "#606060"
    ),
    defaultColDef = colDef(
      align       = "center",
      headerStyle = list(
        fontFamily = "Montserrat, sans-serif",
        fontSize   = "12px",
        textAlign  = "center",
        padding    = "4px 6px"
      ),
      style = list(
        fontFamily = "Montserrat, sans-serif",
        fontSize   = "12px",
        padding    = "4px 6px"
      )
    ),
    ...
  )
}
