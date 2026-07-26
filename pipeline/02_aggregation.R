## Packages ----------------------------------------------------------------------------------------------
#| label: packages
library(tidyverse)
library(here)
library(DBI)
library(RPostgres)


## Clear Env ----------------------------------------------------------------------------------------------
rm(list = setdiff(ls(), c("con", "current_season")))


## DB Connection ------------------------------------------------------------------------------------------
#| label: db_connection

if (!exists("con", where = globalenv(), inherits = FALSE)) {
  source(here("R/db_connection.R"))
}


## DB Tables ----------------------------------------------------------------------------------------------
#| label: db_tables

# Manager ID and names
managers <- dbReadTable(con, "managers")
# Season ID and label
seasons <- dbReadTable(con, "seasons")
# Gameweek ID and date
gameweeks <- dbReadTable(con, "gameweeks")
# Team names
team_names <- dbReadTable(con, "team_names")
# Season standings, and gameweek snapshot for each manager
standings <- dbReadTable(con, "season_standings")
# Squads - all players for each manager, each gameweek
squads <- dbReadTable(con, "squads")
# Transfers - all transfers for each manager, each gameweek
transfers <- dbReadTable(con, "transfers")

# Creating a vector of all tables loaded from DB that can be filtered by season_id
db_tables <- setdiff(ls(), c("con", "current_season", "managers"))

# Filtering for the current_season
filtered <- db_tables |> 
  mget(envir = globalenv()) |> 
  map(\(df) filter(df, season_id == .env$current_season))

# Publish filtered db_tables to global env
list2env(filtered, envir = globalenv())

# Cleanup
rm(filtered)


## Player Summary -----------------------------------------------------------------------------------------
#| label: player_summary

# Creating aggregated stats for each player, across all manager x gameweek instances
player_summary <- squads |> 
  group_by(player) |> 
  summarise(
    owned            = n(),
    started          = sum(manager_selected == "Selected"),
    benched          = sum(manager_selected == "Not Selected"),
    start_pct        = round(started / owned, 2),
    sub_off          = sum(actual_start %in% "Sub Off"),
    sub_off_pct      = round(sub_off / started, 2),
    sub_on           = sum(actual_start %in% "Sub On"),
    sub_on_pct       = round(sub_on / benched, 2),
    played           = sum((started - sub_off) + sub_on),
    played_pct       = round(played / owned, 2),
    remain_bench     = benched - sub_on,
    remain_bench_pct = round(remain_bench / owned, 2),
    played_pts       = sum(points[actual_start %in% c("Played", "Sub On")]),
    avg_played_pts   = round(mean(points[actual_start %in% c("Played", "Sub On")]), 2),
    bench_pts        = sum(points[actual_start == "Bench"]),
    avg_bench_pts    = round(mean(points[actual_start %in% c("Bench", "Sub Off")]), 2),
    captain_n        = sum(captain == "C", na.rm = TRUE),
    vice_n           = sum(captain == "V", na.rm = TRUE),
    played_as_c      = sum(earned_captain),
    no_captain_n     = sum(is.na(captain)) - remain_bench,
    captain_pts      = sum(points[earned_captain]),
    avg_captain_pts  = round(mean(points[earned_captain]), 2),
    captain_xtra     = round(sum(points[earned_captain] / 2), 0),
    captain_pts_pct  = round(captain_pts / played_pts, 2),
    .groups = "drop"
  ) |> 
  mutate(
    played_pct       = ifelse(is.nan(played_pct)       | is.infinite(played_pct),       NA, played_pct),
    sub_off_pct      = ifelse(is.nan(sub_off_pct)      | is.infinite(sub_off_pct),      NA, sub_off_pct),
    sub_on_pct       = ifelse(is.nan(sub_on_pct)       | is.infinite(sub_on_pct),       NA, sub_on_pct),
    avg_played_pts   = ifelse(is.nan(avg_played_pts)   | is.infinite(avg_played_pts),   NA, avg_played_pts),
    avg_bench_pts    = ifelse(is.nan(avg_bench_pts)    | is.infinite(avg_bench_pts),    NA, avg_bench_pts),
    remain_bench_pct = ifelse(is.nan(remain_bench_pct) | is.infinite(remain_bench_pct), NA, remain_bench_pct),
    avg_captain_pts  = ifelse(is.nan(avg_captain_pts)  | is.infinite(avg_captain_pts),  NA, avg_captain_pts),
    captain_pts_pct  = ifelse(is.nan(captain_pts_pct)  | is.infinite(captain_pts_pct),  NA, captain_pts_pct),
    avg_bench_pts    = ifelse(benched == 0, NA, avg_bench_pts)
  )

# Creating a dataframe of all instances a player was used
instances <- squads |> 
  group_by(player) |> 
  nest(all_instances = -player)

# Joining instances to the player summary
player_summary <- instances |> 
  left_join(player_summary, by = "player")

# Cleanup
rm(instances)


## Player Captaincy ---------------------------------------------------------------------------------------
#| label: player_captaincy

# Step 1: All gameweeks where a player earned the armband
captain_instances <- squads |>
  filter(earned_captain) |>
  mutate(was_promoted = captain == "V")

# Step 2: Mini-league average captain points per gameweek (the benchmark)
league_avg_captain <- captain_instances |>
  group_by(gameweek_id) |>
  summarise(
    league_avg_captain_pts = round(mean(points), 2),
    .groups = "drop"
  )

# Step 3: Join average back and calculate differential vs average
captain_instances <- captain_instances |>
  left_join(league_avg_captain, by = "gameweek_id") |>
  mutate(vs_league_avg = points - league_avg_captain_pts)

# Step 4: Flag differential captains (only one manager picked them as C)
captain_counts_per_gw <- squads |>
  filter(captain == "C") |>
  group_by(gameweek_id, player) |>
  summarise(n_managers_captained = n(), .groups = "drop")

captain_instances <- captain_instances |>
  left_join(captain_counts_per_gw, by = c("gameweek_id", "player")) |>
  mutate(
    n_managers_captained = replace_na(n_managers_captained, 0),
    is_differential      = n_managers_captained == 1
  )

# Step 5: Nest raw instances per player
captain_instances_nested <- captain_instances |>
  group_by(player) |>
  nest(captain_instances = -player)

# Step 6: Flat summary metrics per player
captain_summary_flat <- captain_instances |>
  group_by(player) |>
  summarise(
    times_captained    = sum(!was_promoted),
    times_as_vc        = n() - times_captained,
    times_promoted     = sum(was_promoted),
    total_armband_gw   = n(),
    total_pts_returned = sum(points),
    avg_pts_returned   = round(mean(points), 2),
    xtra_pts_generated = round(sum(points / 2), 0),
    promotion_pts      = sum(points[was_promoted]),
    avg_promotion_pts  = round(mean(points[was_promoted]), 2),
    total_vs_avg       = round(sum(vs_league_avg), 2),
    avg_vs_avg         = round(mean(vs_league_avg), 2),
    best_gw_vs_avg     = round(max(vs_league_avg), 2),
    worst_gw_vs_avg    = round(min(vs_league_avg), 2),
    times_differential = sum(is_differential),
    .groups = "drop"
  ) |>
  mutate(
    avg_promotion_pts = ifelse(
      is.nan(avg_promotion_pts) | is.infinite(avg_promotion_pts),
      NA, avg_promotion_pts
    )
  )

# Step 7: Join nested instances onto flat summary
player_captaincy <- captain_instances_nested |>
  left_join(captain_summary_flat, by = "player") |>
  arrange(desc(times_captained))

# Cleanup
rm(
  captain_summary_flat, captain_counts_per_gw, captain_instances, 
   captain_instances_nested, league_avg_captain
)


## Manager Summary ----------------------------------------------------------------------------------------
#| label: manager_summary

# Flat season-level stats per manager
season_flat <- standings |>
  group_by(manager_id) |>
  summarise(
    total_pts    = total[which.max(gameweek_id)],
    max_pts      = max(gw_net),
    min_pts      = min(gw_net),
    mean_pts     = round(mean(gw_net), 1),
    med_pts      = median(gw_net),
    sd_pts       = round(sd(gw_net), 1),
    avg_rank     = round(median(overall_rank), 1),
    tv_growth    = team_value[which.max(gameweek_id)] - 100,
    transfers    = sum(gw_transfers),
    hits         = sum(extra_trf),
    hits_pts     = sum(hit_points),
    .groups = "drop"
  )

# Player-level stats per manager
team_flat <- squads |>
  group_by(manager_id) |>
  summarise(
    unique_players         = n_distinct(player),
    unique_started_players = n_distinct(player[actual_start %in% c("Played", "Sub On")]),
    unique_captains        = n_distinct(player[captain == "C" & !is.na(captain)])
  )

# Nested gameweek history per manager (includes their team each week)
nested_gameweek <- standings |>
  left_join(
    squads |>
      group_by(manager_id, gameweek_id) |>
      nest(gw_team = -c(manager_id, gameweek_id)),
    by = c("manager_id", "gameweek_id")
  ) |>
  group_by(manager_id) |>
  nest(gw_history = -manager_id)

# Nested player summary per manager
nested_player <- squads |>
  group_by(player, manager_id) |>
  summarise(
    owned            = n(),
    started          = sum(manager_selected == "Selected"),
    benched          = sum(manager_selected == "Not Selected"),
    start_pct        = round(started / owned, 2),
    sub_off          = sum(actual_start %in% "Sub Off"),
    sub_off_pct      = round(sub_off / started, 2),
    sub_on           = sum(actual_start %in% "Sub On"),
    sub_on_pct       = round(sub_on / benched, 2),
    played           = sum((started - sub_off) + sub_on),
    played_pct       = round(played / owned, 2),
    remain_bench     = benched - sub_on,
    remain_bench_pct = round(remain_bench / owned, 2),
    played_pts       = sum(points[actual_start %in% c("Played", "Sub On")]),
    avg_played_pts   = round(mean(points[actual_start %in% c("Played", "Sub On")]), 2),
    bench_pts        = sum(points[actual_start == "Bench"]),
    avg_bench_pts    = round(mean(points[actual_start %in% c("Bench", "Sub Off")]), 2),
    captain_n        = sum(captain == "C", na.rm = TRUE),
    vice_n           = sum(captain == "V", na.rm = TRUE),
    played_as_c      = sum(earned_captain),
    no_captain_n     = sum(is.na(captain)) - remain_bench,
    captain_pts      = sum(points[earned_captain]),
    avg_captain_pts  = round(mean(points[earned_captain]), 2),
    captain_xtra     = round(sum(points[earned_captain] / 2), 0),
    captain_pts_pct  = round(captain_pts / played_pts, 2),
    .groups = "drop"
  ) |> 
  group_by(manager_id) |>
  nest(manager_player_summary = -manager_id)

# Combine everything
manager_summary <- nested_gameweek |>
  left_join(nested_player,  by = "manager_id") |>
  left_join(season_flat,    by = "manager_id") |>
  left_join(team_flat,      by = "manager_id")

# Cleanup
rm(nested_gameweek, nested_player, season_flat, team_flat)


## Manager of the Month -----------------------------------------------------------------------------------
#| label: manager_of_the_month

season_month_order <- c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5)

manager_of_the_month <- standings |>
  left_join(gameweeks |> select(gameweek_id, date, month), by = "gameweek_id") |> 
  group_by(month, manager_id) |>
  summarise(month_pts = sum(gw_net), .groups = "drop") |>
  group_by(month) |>
  slice_max(month_pts, n = 1) |>
  ungroup() |>
  mutate(month = factor(month, levels = season_month_order)) |> 
  arrange(month)


## Gameweek Summary ---------------------------------------------------------------------------------------
#| label: gameweek_summary

# Flat gameweek stats
gw_flat <- standings |>
  group_by(gameweek_id) |>
  summarise(
    max_pts          = max(gw_net),
    top_scorer       = manager_id[which.max(gw_net)],
    min_pts          = min(gw_net),
    low_scorer       = manager_id[which.min(gw_net)],
    range_pts        = max_pts - min_pts,
    mean_pts         = round(mean(gw_net), 1),
    med_pts          = median(gw_net),
    sd_pts           = round(sd(gw_net), 1),
    max_to_mean_diff = round(max_pts - mean_pts, 1),
    min_to_mean_diff = round(mean_pts - min_pts, 1),
    transfers        = sum(gw_transfers),
    .groups = "drop"
  )

# Nested state-of-play snapshot per gameweek
gw_nested <- standings |>
  group_by(gameweek_id) |>
  nest(state_of_play = -gameweek_id)

# Nested transfer log per gameweek (with hit costs spread across transfers)
gw_nested_transfers <- transfers |>
  left_join(
    standings |> select(gameweek_id, manager_id, gw_transfers, hit_points),
    by = c("gameweek_id", "manager_id")
  ) |>
  mutate(
    hit_cost_per_transfer = hit_points / gw_transfers,
    net_points_diff       = points_diff + hit_cost_per_transfer
  ) |>
  group_by(gameweek_id) |>
  nest(transfer_log = -gameweek_id)

# Combine and handle gameweeks with no transfers
gameweek_summary <- gw_flat |>
  left_join(gw_nested_transfers, by = "gameweek_id") |>
  left_join(gw_nested,           by = "gameweek_id") |>
  mutate(transfer_log = map(transfer_log, ~ .x %||% tibble()))

# Cleanup
rm(gw_nested, gw_nested_transfers, gw_flat)
