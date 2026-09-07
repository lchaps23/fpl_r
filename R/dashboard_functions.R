source(here("R", "aesthetics", "fpl_table_reactable.R"))

#### ---- HELPER FUNCTIONS ---- ####

# ----
#| label: rank_suffix

rank_suffix <- function(value) {
  suffix <- case_when(
    value %% 100 %in% 11:13 ~ "th",
    value %% 10 == 1        ~ "st",
    value %% 10 == 2        ~ "nd",
    value %% 10 == 3        ~ "rd",
    TRUE                    ~ "th"
  )
  paste0(value, suffix)
}




# ----
#| label: rgba_col
rgba_col <- function(colour, alpha) {
  rgb_vals <- col2rgb(colour)
  sprintf(
    "rgba(%d,%d,%d,%s)",
    rgb_vals["red", 1], rgb_vals["green", 1], rgb_vals["blue", 1], alpha
  )
}




# ----
#| label: rank_variable

rank_variable <- function(df, var, high_to_low = TRUE) {
  
  var_name <- deparse(substitute(var))
  
  rank_num_name  <- paste0(var_name, "_rank_num")
  rank_char_name <- paste0(var_name, "_rank_char")
  tied_name      <- paste0(var_name, "_tied")
  
  if (high_to_low) {
    
    df <- df |> 
      mutate(
        !!rank_num_name  := rank(-{{ var }}, ties.method = "min"),
        !!rank_char_name := rank_suffix(!!sym(rank_num_name)),
        !!tied_name      := duplicated(!!sym(rank_num_name)) | duplicated(!!sym(rank_num_name), fromLast = TRUE)
      )
  }
  
  else {
    
    df <- df |> 
      mutate(
        !!rank_num_name  := rank({{ var }}, ties.method = "max"),
        !!rank_char_name := rank_suffix(!!sym(rank_num_name)),
        !!tied_name      := duplicated(!!sym(rank_num_name)) | duplicated(!!sym(rank_num_name), fromLast = TRUE)
      )
  }
}




#### ---- PLOT FUNCTIONS ---- ####

# ----
#| label: league_position

league_position_plot <- function(data) {

n_managers <- data$standings |>
  filter(gameweek_id == max(gameweek_id)) |>
  distinct(manager_id) |>
  nrow()

p <- plot_ly()

for (m in unique(data$standings$manager_id)) {
  
  manager_data <- data$standings |>
    filter(manager_id == m)
  
  p <- p |>
    add_trace(
      data = manager_data,
      x = ~gameweek_id,
      y = ~season_standing,
      customdata = ~comma(total),
      type = "scatter",
      mode = "lines+markers",
      text = ~display_name,
      name = ~display_name,
      line = list(
        color = ~colour_hex,
        width = 3,
        smoothing = 1.3
      ),
      marker = list(
        size = 9,
        color = ~colour_hex
      ),
      hovertemplate = paste(
        "<i><b>%{text}</b></i><br>",
        "<b>GW: </b>%{x}<br>",
        "<b>Position: </b>%{y}<br>",
        "<b>Points: </b>%{customdata}<extra></extra>"
      )
    )
}

p |>
  layout(
    font = list(
      color = "#F2F2F2",
      family = "Montserrat"
    ),
    
    xaxis = list(
      title = "",
      color = "#F2F2F2",
      tickfont = list(color = "#F2F2F2"),
      tickvals = c(1, seq(5, 38, by = 5), 38),
      range = c(0, 38.5),
      gridcolor = "#3A3A3A",
      zerolinecolor = "#3A3A3A"
    ),
    
    yaxis = list(
      title = "Position",
      autorange = "reversed",
      color = "#F2F2F2",
      tickfont = list(color = "#F2F2F2"),
      titlefont = list(color = "#F2F2F2"),
      tickvals = 1:n_managers,
      ticktext = rank_suffix(1:n_managers),
      gridcolor = "#3A3A3A",
      zerolinecolor = "#3A3A3A"
    ),
    
    legend = list(
      orientation = "h",
      x           = 0.5,
      xanchor     = "center",
      y           = -0.1,
      font        = list(color = "#F2F2F2")
    ),
    
    margin = list(
      t = 0
    ),
    
    showlegend = TRUE,
    plot_bgcolor = "#232323",
    paper_bgcolor = "#232323"
  ) |> 
  config(displayModeBar = FALSE)
}




# ----
#| label: relative_performance

relative_performance_plot <- function(data) {
  
  p <- plot_ly()
  
  p <- p |>
    add_segments(
      x = min(data$standings$gameweek_id),
      xend = max(data$standings$gameweek_id),
      y = 0,
      yend = 0,
      line = list(
        dash = "dash",
        color = "#F2F2F2"
      ),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = "none"
    )
  
  for (m in unique(data$standings$manager_id)) {
    
    manager_data <- data$standings |>
      filter(manager_id == m)
    
    p <- p |>
      add_trace(
        data = manager_data,
        x = ~gameweek_id,
        y = ~cumulative_z,
        customdata = ~gw_z_score,
        type = "scatter",
        mode = "lines+markers",
        text = ~display_name,
        name = ~display_name,
        line = list(
          color = ~colour_hex,
          width = 2
        ),
        marker = list(
          size = 7,
          color = ~colour_hex
        ),
        hovertemplate =
          paste(
            "<i><b>%{text}</b></i><br>",
            "<b>GW:</b> %{x}<br>",
            "<b>Z-Score:</b> %{customdata}</br>",
            "<b>Total Z-Score:</b> %{y:.2f}<extra></extra>"
          )
      )
  }
  
  p |>
    layout(
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title = "",
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        tickvals = c(1, seq(5, 38, by = 5), 38),
        range = c(0, 38.5),
        gridcolor = "#3A3A3A",
        zerolinecolor = "#3A3A3A"
      ),
      
      yaxis = list(
        title = "Cumulative Z-Score",
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A",
        zerolinecolor = "#3A3A3A"
      ),
      
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.1,
        font        = list(color = "#F2F2F2")
      ),
      
      margin = list(
        t = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor = "#232323"
    ) |> 
    config(displayModeBar = FALSE)
}




# ----
#| label: gw_score_distribution

gw_score_distribution_plot <- function(data) {
  
  dist_data <- data$standings |>
    group_by(display_name) |>
    mutate(median_score = median(gw_net)) |>
    ungroup()
  
  mgr_order <- dist_data |>
    distinct(display_name, median_score) |>
    arrange(desc(median_score)) |>
    pull(display_name)
  
  p <- plot_ly()
  
  for (m in mgr_order) {
    
    d   <- filter(dist_data, display_name == m)
    col <- d$colour_hex[1]
    
    p <- add_trace(p,
                   data      = d,
                   y         = ~gw_net,
                   x         = ~display_name,
                   type      = "violin",
                   side      = "positive",
                   name      = m,
                   box       = list(visible = TRUE),
                   meanline  = list(visible = FALSE),
                   points    = "all",
                   jitter    = 0.4,
                   pointpos  = -0.6,
                   marker    = list(color = rgba_col(col, 0.80), size = 7),
                   line      = list(color = rgba_col(col, 1.00), width = 1),
                   fillcolor = rgba_col(col, 0.30),
                   text      = ~paste0(
                     "<i><b>", display_name, "</b></i><br>",
                     "<b>GW: </b>", gameweek_id, "<br>",
                     "<b>Pts: </b>", gw_net),
                   hoverinfo  = "text",
                   showlegend = FALSE
    )
  }
  
  p |> 
    layout(
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      yaxis = list(
        title     = "Net GW Score",
        tickvals  = seq(0, 140, by = 10),
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A"
      ),
      
      xaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = mgr_order,
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2")
      ),
      
      margin = list(
        t = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323"
    ) |>
    config(displayModeBar = FALSE)
}




# ----
#| label: captainless_fpl

captainless_plot <- function(data) {
  
  captainless_fpl <- data$manager_summary |>
    ungroup() |>
    mutate(
      total_captain_xtra = map_dbl(manager_player_summary,
                                   ~ sum(.x$captain_xtra, na.rm = TRUE)),
      total_without_cpt  = total_pts - total_captain_xtra,
      prop = round(total_captain_xtra / total_pts, 2)
    ) |>
    select(manager_id, display_name, colour_hex, total_pts, total_captain_xtra, total_without_cpt, prop)
  
  cpt_order <- captainless_fpl |>
    arrange(desc(total_without_cpt)) |>
    pull(display_name)
  
  # Axis bounds derived from the data itself, so the chart works for any
  # season regardless of that season's actual point totals.
  y_min <- 0
  y_max <- ceiling(max(captainless_fpl$total_pts) / 100) * 100 + 100
  
  p <- plot_ly()
  
  for (m in cpt_order) {
    
    d   <- filter(captainless_fpl, display_name == m)
    col <- d$colour_hex[1]   # this manager's colour, straight from the data
    
    # Ghost bar: full total_pts at low opacity, label sits above it
    p <- add_bars(p,
                  x            = d$display_name,
                  y            = d$total_pts,
                  marker       = list(color = rgba_col(col, 0.3)),
                  text         = comma(d$total_pts),
                  textposition = "outside",
                  textfont     = list(color = "#F2F2F2", size = 12, weight = "bold"),
                  hoverinfo    = "none",
                  showlegend   = FALSE
    )
    
    p <- add_bars(p,
                  x            = d$display_name,
                  y            = d$total_without_cpt,
                  marker       = list(color = rgba_col(col, 1.0)),
                  text         = comma(d$total_without_cpt),
                  textposition = "inside",
                  textfont     = list(color = "white", size = 12, weight = "bold"),
                  hoverinfo    = "none",
                  showlegend   = FALSE
    )
  }
  
  cpt_labels <- captainless_fpl |>
    arrange(desc(total_without_cpt)) |>
    mutate(
      label_y   = total_without_cpt + (total_captain_xtra / 2),
      label_txt = paste0("+", total_captain_xtra)
    )
  
  p <- add_trace(p,
                 data       = cpt_labels,
                 x          = ~display_name,
                 y          = ~label_y,
                 type       = "scatter",
                 mode       = "text",
                 text       = ~label_txt,
                 textfont   = list(color = "#F2F2F2", size = 12, weight = "bold"),
                 hoverinfo  = "none",
                 showlegend = FALSE
  )
  
  p |>
    layout(
      barmode = "overlay",
      
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = cpt_order,
        color         = "#F2F2F2",
        tickfont      = list(color = "#F2F2F2")
      ),
      
      yaxis = list(
        title      = "Points",
        range      = c(y_min, y_max),
        tickvals   = seq(y_min, y_max, by = 250),
        tickformat = ",",
        color      = "#F2F2F2",
        tickfont   = list(color = "#F2F2F2"),
        titlefont  = list(color = "#F2F2F2"),
        gridcolor  = "#3A3A3A"
      ),
      
      margin = list(
        t = 0,
        b = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323",
      showlegend    = TRUE
    ) |>
    config(displayModeBar = FALSE)
}




# ----
#| label: captain_accuracy

captain_accuracy_plot <- function(data) {

  all_captain_pick_accuracy <- data$squads |>
    filter(actual_start == "Played") |>
    left_join(
      data$standings |> select(manager_id, gameweek_id, chip),
      by = c("manager_id", "gameweek_id")
    ) |>
    mutate(
      multiplier  = if_else(chip %in% c("TC1", "TC2"), 3, 2),
      base_points = if_else(earned_captain == 1, points / multiplier, points)
    ) |>
    group_by(manager_id, gameweek_id) |>
    slice_max(base_points, n = 1, with_ties = FALSE) |>
    ungroup() |>
    group_by(manager_id) |>
    summarise(
      correct_cpt_pick = sum(earned_captain),
      gameweeks        = n(),
      prop             = round(correct_cpt_pick / gameweeks, 2)
    ) |>
    arrange(desc(prop)) |>
    left_join(data$managers, by = "manager_id")

  df <- all_captain_pick_accuracy |>
    arrange(correct_cpt_pick)

  p <- plot_ly()

  p <- p |>
    add_trace(
      data = df,
      x = ~correct_cpt_pick,
      y = ~display_name,
      type = "bar",
      orientation = "h",
      marker = list(color = ~colour_hex),
      text = ~paste0(correct_cpt_pick, ", (", percent(prop), ")"),
      textposition = "outside",
      textfont = list(color = "#F2F2F2", size = 10),
      hovertemplate = paste(
        "<i><b>%{y}</b></i><br>",
        "<b>Optimal Picks:</b> %{x}<br>", "<extra></extra>"
      ),
      showlegend = FALSE
    )

  p |>
    layout(
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title = "Weeks with optimal captain picks",
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A",
        rangemode = "tozero",
        range = c(-0.2, max(df$correct_cpt_pick) * 1.15)
      ),
      
      yaxis = list(
        title = "",
        color = "#F2F2F2",
        tickfont = list(color = "#F2F2F2"),
        categoryorder = "array",
        categoryarray = df$display_name
      ),
      
      margin = list(
        t = 0
      ),
      
      plot_bgcolor = "#232323",
      paper_bgcolor = "#232323"
    ) |> 
    config(displayModeBar = FALSE)
}




# ----
#| label: free_hit

free_hit_plot <- function(data) {
  
  fh_data <- data$standings |>
    filter(chip %in% c("FH1", "FH2"))
  
  mgr_order <- fh_data |>
    group_by(display_name) |>
    summarise(total = sum(gw_net)) |>
    arrange(total) |>
    pull(display_name)
  
  fh1_data <- filter(fh_data, chip == "FH1")
  fh2_data <- filter(fh_data, chip == "FH2")
  
  p <- plot_ly()
  
  p <- p |>
    add_bars(
      data             = fh1_data,
      x                = ~gw_net,
      y                = ~display_name,
      orientation      = "h",
      name             = "FH1",
      marker           = list(color = "#5DCAA5"),
      text             = ~gw_net,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>FH1: </b>", gw_net, "<extra></extra>"
      )
    )
  
  p <- p |>
    add_bars(
      data             = fh2_data,
      x                = ~gw_net,
      y                = ~display_name,
      orientation      = "h",
      name             = "FH2",
      marker           = list(color = "#F0A500"),
      text             = ~gw_net,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>FH2: </b>", gw_net, "<extra></extra>"
      )
    )
  
  p |>
    layout(
      barmode = "stack",
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title     = "",
        color     = "#F2F2F2",
        tickfont  = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A",
        range = c(-2, 150)
      ),
      
      yaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = mgr_order,
        color         = "#F2F2F2",
        tickfont      = list(color = "#F2F2F2")
      ),
      
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.05,
        font        = list(color = "#F2F2F2")
      ),
      
      margin = list(
        t = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323"
    ) |>
    config(displayModeBar = FALSE)
}




# ----
#| label: triple_captain

triple_captain_plot <- function(data) {
  
  tc_data <- data$squads |>
    filter(actual_start == "Played") |>
    left_join(
      data$standings |> select(manager_id, gameweek_id, chip),
      by = c("manager_id", "gameweek_id")
    ) |>
    filter((chip == "TC1" | chip == "TC2") & earned_captain == TRUE)
  
  tc_mgr_order <- tc_data |>
    group_by(display_name) |>
    summarise(total_tc = sum(points)) |>
    arrange(desc(total_tc)) |>
    pull(display_name)
  
  tc1_data <- tc_data |> filter(chip == "TC1")
  tc2_data <- tc_data |> filter(chip == "TC2")
  
  p <- plot_ly()
  
  p <- p |>
    add_bars(
      data             = tc1_data,
      x                = ~points,
      y                = ~display_name,
      orientation      = "h",
      name             = "TC1",
      marker           = list(color = "#5DCAA5"),
      text             = ~points,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>Player: </b>", player, "<br>",
        "<b>TC1: </b>", points, "<extra></extra>"
      )
    )
  
  p <- p |>
    add_bars(
      data             = tc2_data,
      x                = ~points,
      y                = ~display_name,
      orientation      = "h",
      name             = "TC2",
      marker           = list(color = "#F0A500"),
      text             = ~points,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>Player: </b>", player, "<br>",
        "<b>TC2: </b>", points, "<extra></extra>"
      )
    )
  
  p |>
    layout(
      barmode = "stack",
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title     = "",
        color     = "#F2F2F2",
        tickfont  = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A",
        range = c(-1, 100)
      ),
      
      yaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = rev(tc_mgr_order),
        color         = "#F2F2F2",
        tickfont      = list(color = "#F2F2F2")
      ),
      
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.05,
        font        = list(color = "#F2F2F2")
      ),
      
      margin = list(
        t = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323"
    ) |>
    config(displayModeBar = FALSE)
}



# ----
#| label: bench_boost

bench_boost_plot <- function(data) {
  
  bb_data <- data$squads |>
    group_by(gameweek_id, display_name, manager_id) |>
    summarise(
      bb_points = sum(points[player_slot %in% 12:15 & actual_start == "Played"]),
      gw_points = sum(points[actual_start %in% c("Played", "Sub On")]),
      no_bb_points = gw_points - bb_points
    ) |>
    mutate(
      bb = case_when(
        gameweek_id <= 19 & bb_points > 0 ~ "BB1",
        gameweek_id >  19 & bb_points > 0 ~ "BB2",
        TRUE ~ NA
      ),
      prop = round(bb_points / gw_points, 2)
    ) |>
    filter(bb == "BB1" | bb == "BB2") |>
    ungroup()
  
  bb_data <- bb_data |>
    left_join(data$managers |> select(manager_id, colour_hex), by = "manager_id")
  
  bb_mgr_order <- bb_data |>
    group_by(display_name) |>
    summarise(total_bb = sum(bb_points)) |>
    arrange(desc(total_bb)) |>
    pull(display_name)
  
  bb1_data <- bb_data |> filter(bb == "BB1")
  bb2_data <- bb_data |> filter(bb == "BB2")
  
  p <- plot_ly()
  
  p <- p |>
    add_bars(
      data             = bb1_data,
      x                = ~bb_points,
      y                = ~display_name,
      orientation      = "h",
      name             = "BB1",
      marker           = list(color = "#5DCAA5"),
      text             = ~bb_points,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>BB1: </b>", bb_points, "<extra></extra>"
      )
    )
  
  p <- p |>
    add_bars(
      data             = bb2_data,
      x                = ~bb_points,
      y                = ~display_name,
      orientation      = "h",
      name             = "BB2",
      marker           = list(color = "#F0A500"),
      text             = ~bb_points,
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "#4D4D4D", size = 14),
      hovertemplate    = ~paste0(
        "<i><b>", display_name, "</b></i><br>",
        "<b>GW: </b>", gameweek_id, "<br>",
        "<b>BB2: </b>", bb_points, "<extra></extra>"
      )
    )
  
  p |>
    layout(
      barmode = "stack",
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title     = "",
        color     = "#F2F2F2",
        tickfont  = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A",
        range = c(-0.8, 50)
      ),
      
      yaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = rev(bb_mgr_order),
        color         = "#F2F2F2",
        tickfont      = list(color = "#F2F2F2")
      ),
      
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.05,
        font        = list(color = "#F2F2F2")
      ),
      
      margin = list(
        t = 0
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323"
    ) |>
    config(displayModeBar = FALSE)
}




# ----
#| label: transfer_heatmap

transfer_activity_heatmap <- function(data) {
  
  all_transfer_activity <- data$standings |>
    select(display_name, gameweek_id, gw_transfers) |>
    pivot_wider(
      id_cols = display_name,
      names_from = gameweek_id,
      values_from = gw_transfers,
      values_fill = 0
    )
  
  heatmap_managers <- all_transfer_activity$display_name
  
  heatmap_matrix <- all_transfer_activity |>
    select(-display_name) |>
    as.matrix()
  
  text_matrix <- heatmap_matrix
  text_matrix[text_matrix == 0] <- ""
  
  # Six discrete colours, one per transfer count 0-5 (viridis palette,
  # matching the ones you'd already picked out for the continuous version).
  bucket_colours <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#d4c700")
  n_buckets <- length(bucket_colours)
  
  # Two stops per colour (start and end of its band) is what forces a hard
  # step instead of a gradient - plotly always interpolates smoothly
  # *between* stops, so giving it nowhere to interpolate to (same colour on
  # both ends of a band) is what makes each band flat.
  discrete_colorscale <- map(seq_len(n_buckets), \(i) {
    list(
      list((i - 1) / n_buckets, bucket_colours[i]),
      list(i / n_buckets,       bucket_colours[i])
    )
  }) |>
    flatten()
  
  plot_ly(
    x          = as.numeric(colnames(heatmap_matrix)),
    y          = heatmap_managers,
    z          = heatmap_matrix,
    type       = "heatmap",
    zmin       = 0,
    zmax       = n_buckets - 1,
    text       = text_matrix,
    texttemplate = "%{text}",
    textfont = list(color = "#232323"),
    colorscale = discrete_colorscale,
    colorbar = list(
      title      = "Transfers",
      tickfont   = list(color = "#F2F2F2"),
      titlefont  = list(color = "#F2F2F2"),
      tickvals   = 0:(n_buckets - 1),
      dtick      = 1
    ),
    hovertemplate = paste0(
      "<i><b>%{y}</b></i><br>",
      "GW: %{x}<br>",
      "Transfers: %{z}",
      "<extra></extra>"
    )
  ) |>
    layout(
      font = list(
        color = "#F2F2F2",
        family = "Montserrat"
      ),
      
      xaxis = list(
        title     = "Gameweek",
        color     = "#F2F2F2",
        tickfont  = list(color = "#F2F2F2"),
        titlefont = list(color = "#F2F2F2"),
        tickvals  = c(1, seq(5, 38, by = 5), 38),
        gridcolor = "#3A3A3A"
      ),
      
      yaxis = list(
        title     = "",
        color     = "#F2F2F2",
        tickfont  = list(color = "#F2F2F2"),
        gridcolor = "#3A3A3A"
      ),
      
      paper_bgcolor = "#232323",
      plot_bgcolor  = "#232323"
    ) |> 
    config(displayModeBar = FALSE)
}



#### ---- TABLE FUNCTIONS ---- ####
worst_captain_table <- function(data) {
  
  all_cpt_optimal_actual <- data$squads |>
    filter(actual_start == "Played") |>
    left_join(
      data$standings |> select(manager_id, gameweek_id, chip),
      by = c("manager_id", "gameweek_id")
    ) |>
    mutate(
      multiplier  = if_else(chip %in% c("TC1", "TC2"), 3, 2),
      base_points = if_else(earned_captain == 1, points / multiplier, points)
    ) |>
    group_by(manager_id, gameweek_id) |>
    mutate(
      optimal_captain_pts = max(base_points) * multiplier,
      actual_captain_pts  = sum(points[earned_captain == 1]),
      optimal_player      = player[which.max(base_points)],
      actual_player       = player[earned_captain == 1],
      pts_left_on_table   = optimal_captain_pts - actual_captain_pts
    ) |>
    filter(earned_captain == 1) |>
    ungroup()
  
  all_mgr_worst_cpt <- all_cpt_optimal_actual |>
    group_by(display_name) |>
    slice_max(pts_left_on_table, n = 1, with_ties = FALSE) |>
    arrange(desc(pts_left_on_table)) |>
    ungroup() |>
    rank_variable(var = pts_left_on_table, high_to_low = TRUE) |>
    select(pts_left_on_table_rank_char, display_name, gameweek_id, actual_player, actual_captain_pts, optimal_player, optimal_captain_pts, pts_left_on_table)
  
  all_mgr_worst_cpt |>
    fpl_table(
      columns = list(
        pts_left_on_table_rank_char = colDef(
          name = "Rank",
          maxWidth = 100
        ),
        
        display_name = colDef(
          name = "Manager",
          maxWidth = 100
        ),
        
        gameweek_id = colDef(
          name = "GW",
          maxWidth = 70
        ),
        
        actual_player = colDef(
          name = "Player",
          maxWidth = 150
        ),
        
        actual_captain_pts = colDef(
          name = "Points",
          maxWidth = 70
        ),
        
        optimal_player = colDef(
          name = "Player",
          maxWidth = 150
        ),
        
        optimal_captain_pts = colDef(
          name = "Points",
          maxWidth = 70
        ),
        
        pts_left_on_table = colDef(
          name = "Difference",
          maxWidth = 100
        )
      ),
      
      columnGroups = list(
        colGroup(name = "Actual Captain", columns = c("actual_player", "actual_captain_pts")),
        colGroup(name = "Optimal Captain", columns = c("optimal_player", "optimal_captain_pts"))
      )
    )
}