
# Manager Aesthetics for all outputs
## Setting consistent manager colour palette for all seasons

all_managers <- c("luke", "jameslm", "jamesc", "fred", "tim", "amy", "lewis", "andy")

build_manager_colours <- function() {
  
  c(
    "luke" = "#0072B5",
    "lewis" = "#BC3C29",
    "amy" = "#F48FB1",
    "jamesc" = "#6F99AD",
    "jameslm" = "#20854E",
    "tim" = "#E18727",
    "andy" = "#8D6E63",
    "fred" = "goldenrod"
  )
  
}

manager_colours <- build_manager_colours()


# Converting manager names from machine-readable to human-readable.
# To be used for all outputs.

manager_display_names <- c(
  "luke" = "Luke", 
  "jamesc" = "James C",
  "jameslm" = "James LM",
  "amy" = "Amy",
  "tim" = "Tim",
  "fred" = "Fred",
  "andy" = "Andy",
  "lewis" = "Lewis"
)

output_manager_names <- function (x) {
  unname(manager_display_names[x])
}

# Output templates
## ggplot2 visualisations

theme_fpl <- function(grid = "x") {
  
  base <- theme_minimal() +
    
    theme(
      
      # --- Text ---
      text = element_text(colour = "grey30"),
      
      # --- Grid lines ---
      panel.grid = element_blank(),
      
      # --- Background ---
      plot.background = element_rect(fill = "oldlace", colour = "grey30"),
      
      # --- Titles & Caption ---
      plot.title.position = "plot",
      plot.title          = element_text(face = "bold", margin = margin(b = 5)),
      plot.subtitle       = element_text(margin = margin(b = 10)),
      # Left-aligns the caption (default is right)
      plot.caption        = element_text(hjust = 0),
      
      # --- Axis titles ---
      axis.title.x = element_text(margin = margin(t = 5), size = 10),
      axis.title.y = element_text(margin = margin(r = 5), size = 10),
      
      # --- Legend titles ---
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 10)
      
    )
  
  # Now selectively add back gridlines depending on what the user asks for.
  # The `grid` argument controls which axis gets dashed gridlines:
  #   "x"  → vertical dashed lines (good for horizontal bar/lollipop charts)
  #   "y"  → horizontal dashed lines (good for vertical bar charts)
  #   "xy" → both (good for scatter plots)
  
  line_style <- element_line(colour = "#e3e1e1", linetype = "dashed")
  
  if (grid == "x") {
    base <- base + theme(panel.grid.major.x = line_style)
  } else if (grid == "y") {
    base <- base + theme(panel.grid.major.y = line_style)
  } else if (grid == "xy") {
    base <- base + theme(
      panel.grid.major.x = line_style,
      panel.grid.major.y = line_style
    )
  }
  
  base  # Return the finished theme
  
}

## gt tables

theme_fpl_gt <- function(gt_table, width = 400) {
  
  gt_table |>
    cols_align(align = "center", columns = everything()) |>
    tab_style(
      style     = list(cell_fill(color = "grey95"), cell_text(weight = "bold")),
      locations = cells_body()
    ) |>
    tab_style(
      style     = cell_text(weight = "bold", size = "large"),
      locations = cells_title(groups = "title")
    ) |>
    tab_options(
      table.font.names          = "Arial",
      table.border.top.style    = "hidden",
      column_labels.font.weight = "bold",
      table.width               = width
    )
}


## Scorecard highlighter

highlight_manager <- function(data, mgr, col_var = "manager") {
  data |>
    mutate(
      .alpha     = if_else(.data[[col_var]] == mgr, 1, 0.6),
      .linewidth = if_else(.data[[col_var]] == mgr, 1, 0.5),
      .size      = if_else(.data[[col_var]] == mgr, 3, 1.5)
    )
}
