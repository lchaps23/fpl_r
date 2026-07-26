## Packages ----------------------------------------------------------------------------------------------
#| label: packages
library(tidyverse)
library(readxl)
library(here)
library(DBI)
library(RPostgres)
library(validate)


## Parameters ----------------------------------------------------------------------------------------------
#| label: parameters
read_path <- here("data", paste0("season_", current_season, ".xlsx"))
  

## DB Connection ------------------------------------------------------------------------------------------
#| label: db_connection
source(here("R/db_connection.R"))


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


## Import Season ------------------------------------------------------------------------------------------
#| label: import_season
raw_season <- read_excel(read_path, sheet = "season")


## Import Teams -------------------------------------------------------------------------------------------
#| label: import_teams
season_managers <- excel_sheets(read_path) |> 
  discard(~ .x %in% c("season", "transfers", "week_1_teams"))

team_list <- map(
    season_managers, ~ read_excel(read_path, sheet = .x) |> 
    mutate(manager = str_replace_all(.x, "_", " "))
  )

raw_teams <- bind_rows(team_list)

rm(team_list, season_managers)


## Import Transfers ---------------------------------------------------------------------------------------
#| label: import_transfers
raw_transfers <- read_excel(read_path, sheet = "transfers")


## Import GW1 Teams ---------------------------------------------------------------------------------------
#| label: import_gw1_teams
raw_gw1_teams <- read_excel(read_path, sheet = "week_1_teams")


## calc_ft() ----------------------------------------------------------------------------------------------
#| label: calc_ft_helper_function
source(here("R/calc_ft.R"))


## Clean Season -------------------------------------------------------------------------------------------
#| label: clean_season
clean_season <- raw_season |> 
  mutate(
    full_name = str_extract(team, "(?<=\\()[^)]+(?=\\))"),
    season_id = current_season
  ) |> 
  left_join(managers |> select(manager_id, manager_full_name), by = c("full_name" = "manager_full_name")) |> 
  select(-c(date, full_name, team)) |> 
  relocate(manager_id, .after = gameweek_id)

# Apply calc_ft
clean_season <- clean_season |> 
  group_split(manager_id) |> 
  map(calc_ft, afcon_trf = TRUE) |> 
  list_rbind()

# Apply gameweek-level z-scores
clean_season <- clean_season |> 
  group_by(gameweek_id) |> 
  mutate(gw_z_score = round((gw_net - mean(gw_net)) / sd(gw_net), 2)) |> 
  ungroup() |> 
  relocate(gw_z_score, .after = gw_net)
  
# Cumulative z-score (running total over season)
clean_season <- clean_season |> 
  group_by(manager_id) |> 
  mutate(cumulative_z = round(cumsum(gw_z_score), 2)) |> 
  ungroup() |> 
  relocate(cumulative_z, .after = gw_z_score)

clean_season <- clean_season |> 
  select(season_id, gameweek_id, manager_id, season_standing, gw_net, gw_z_score, total, cumulative_z, everything())


## Clean Teams ---------------------------------------------------------------------------------------------
#| label: clean_teams
clean_teams <- raw_teams |> 
  mutate(
    season_id = current_season,
    minutes = case_when(
      str_detect(status, "Did not play") ~ 0,
      TRUE ~ as.numeric(str_extract(status, "\\d+"))
    ) |> as.numeric(),
    manager_selected = case_when(
      str_detect(player, "keyboard_arrow_down") ~ "Selected",
      str_detect(player, "keyboard_arrow_up")   ~ "Not Selected",
      player_slot <= 11                         ~ "Selected",
      TRUE                                      ~ "Not Selected"
    ),
    actual_start = case_when(
      str_detect(player, "keyboard_arrow_down") ~ "Sub Off",
      str_detect(player, "keyboard_arrow_up")   ~ "Sub On",
      player_slot <= 11                         ~ "Played",
      TRUE                                      ~ "Bench"
    ),
    player = str_trim(
      str_remove_all(
        player,
        "keyboard_arrow_up\\s*|keyboard_arrow_down\\s*|\\s*lens error"
    ))
  ) |> 
  left_join(managers |> select(manager_id, manager_name), by = c("manager" = "manager_name")) |> 
  select(
    season_id, gameweek_id, manager_id, player_slot, player, captain, points, minutes,
    manager_selected, actual_start, importance
  )

# BB chip correction
# Identify which manager/gameweek combinations used Bench Boost
bb_gameweeks <- clean_season |> 
  filter(chip %in% c("BB1", "BB2")) |> 
  distinct(manager_id, gameweek_id) |> 
  mutate(is_bb_week = TRUE)

# Join that flag onto clean_teams, then recode bench players
# who actually counted as "selected" and "played" during Bench Boost
clean_teams <- clean_teams |> 
  left_join(bb_gameweeks, by = c("manager_id", "gameweek_id")) |> 
  mutate(
    is_bb_week   = replace_na(is_bb_week, FALSE),
    is_bench_bb  = is_bb_week & player_slot >= 12,
    manager_selected = if_else(is_bench_bb, "Selected", manager_selected),
    actual_start      = if_else(is_bench_bb, "Played", actual_start)
  ) |> 
  select(-is_bb_week, -is_bench_bb)
  
# Captain / Earned Captain flags
clean_teams <- clean_teams |> 
  group_by(manager_id, gameweek_id) |> 
  mutate(
    c_played = any(captain == "C" & actual_start == "Played", na.rm = TRUE),
    earned_captain = case_when(
      captain == "C" & c_played  & actual_start == "Played" ~ TRUE,
      captain == "V" & !c_played & actual_start == "Played" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |> 
  ungroup() |> 
  select(-c_played)

rm(bb_gameweeks)


## Clean Transfers ---------------------------------------------------------------------------------------------
#| label: clean_transfers
clean_transfers <- raw_transfers |>  
  mutate(
    points_diff = points_in - points_out,
    season_id = current_season
  ) |> 
  left_join(managers |> select(manager_id, manager_name), by = c("manager" = "manager_name")) |> 
  select(season_id, gameweek_id, manager_id, everything(), -c(Column1, manager))


## Import Rulesets ---------------------------------------------------------------------------------------------
#| label: import_rulesets
source(here("R/validation_rulesets.R"))


## Season Rules Check ------------------------------------------------------------------------------------------
#| label: season_rules_check
# Applies ruleset
season_rules_check <- confront(clean_season, season_rules)

# Outputs ruleset results
summary(season_rules_check) |> 
  as_tibble()

# Checks results for failures and stops if failures found
season_failures <- summary(season_rules_check) |> 
  as_tibble() |>
  filter(fails > 0)

if (nrow(season_failures) > 0) {
  stop(
    paste0(
      "Season validation failed. ",
      nrow(season_failures), " rule(s) failed: ",
      paste(season_failures$name, collapse = ", ")
    )
  )
} else {
  cat("✓ All season validation rules passed!\n")
}

# Checks that the total points matches the sum of gw_net for each manager
total_check <- clean_season |> 
  group_by(manager_id) |> 
  summarise(
    recorded_total = total[which.max(gameweek_id)],
    sum_gw_net     = sum(gw_net),
    match          = recorded_total == sum_gw_net
  )

if (all(total_check$match)) {
  cat("✓ All manager total points match the sum of their weekly scores!\n")
} else {
  
  failed <- total_check |> filter(!match)
  
  stop(
    paste0(
      "Total points mismatch for: ",
      paste(failed$manager, collapse = ", ")
    )
  )
}


## Teams Rules Check -------------------------------------------------------------------------------------------
#| label: teams_rules_check
# Applies ruleset
teams_rules_check <- confront(clean_teams, teams_rules)

# Outputs ruleset results
summary(teams_rules_check) |> 
  as_tibble()

# Checks results for failures and stops if failures found
teams_failures <- summary(teams_rules_check) |> 
  as_tibble() |> 
  filter(fails > 0)

if (nrow(teams_failures) > 0) {
  stop(
    paste0(
      "Teams validation failed. ",
      nrow(teams_failures), " rule(s) failed: ",
      paste(teams_failures$name, collapse = ", ")
    )
  )
} else {
  cat("✓ All teams validation rules passed!\n")
}

# Checks that each manager has 1x Captain and 1x Vice-Captain per gameweek
captain_check <- clean_teams |> 
  group_by(manager_id, gameweek_id) |> 
  summarise(
    n_captain = sum(captain == "C", na.rm = TRUE),
    n_vice    = sum(captain == "V", na.rm = TRUE),
    .groups   = "drop"
  ) |> 
  mutate(valid = n_captain == 1 & n_vice == 1)

if (all(captain_check$valid)) {
  cat("✓ Every manager has exactly 1x Captain and 1x Vice-Captain for each gameweek!\n")
} else {
  
  failed <- captain_check |> filter(!valid)
  
  stop(
    paste0(
      "Captaincy issues found in ", nrow(failed), " gameweek(s):\n",
      paste(
        paste0("GW", failed$gameweek_id, " - ", failed$manager,
               " (C: ", failed$n_captain, ", V: ", failed$n_vice, ")"),
        collapse = "\n"
      )
    )
  )
}


## Transfers Rules Check -----------------------------------------------------------------------------------------
#| label: transfers_rules_check
# Applies ruleset
transfers_rules_check <- confront(clean_transfers, transfers_rules)

# Outputs ruleset results
summary(transfers_rules_check) |> 
  as_tibble()

# Checks results for failures and stops if failures found
transfers_failures <- summary(transfers_rules_check) |> 
  as_tibble() |> 
  filter(fails > 0)

if (nrow(transfers_failures) > 0) {
  stop(
    paste0(
      "Transfers validation failed. ",
      nrow(transfers_failures), " rule(s) failed: ",
      paste(transfers_failures$name, collapse = ", ")
    )
  )
} else {
  cat("✓ All transfers validation rules passed!\n")
}


## Cross-Dataset Checks -----------------------------------------------------------------------------------------
#| label: checking_transfers_match
# Checking gw points across teams and seasons
player_points <- clean_teams |> 
  filter(actual_start == "Played" | actual_start == "Sub On") |> 
  group_by(gameweek_id, manager_id) |> 
  summarise(
    gw_points = sum(points),
    .groups = "drop"
  )

player_points <- player_points |> 
  left_join(clean_season |> select(gameweek_id, manager_id, hit_points, gw_net), by = c("gameweek_id", "manager_id")) |> 
  mutate(teams_points = gw_points + hit_points)

points_mismatches <- player_points |> 
  filter(teams_points != gw_net)

if (nrow(points_mismatches) == 0) {
  cat("✓ All gameweek points match!\n")
} else {
  stop(
    paste0(
      "Points mismatch in ", nrow(points_mismatches), " gameweek(s):\n",
      paste(
        paste0("GW", points_mismatches$gameweek_id, " - ", points_mismatches$manager_id,
               " (teams: ", points_mismatches$teams_points,
               ", season: ", points_mismatches$gw_net, ")"),
        collapse = "\n"
      )
    )
  )
}

# Checks transfers match with season
player_transfers <- clean_transfers |> 
  group_by(gameweek_id, manager_id) |> 
  summarise(
    manual_gw_transfers = n(),
    .groups = "drop"
  )

player_transfers <- player_transfers |> 
  left_join(clean_season |> select(gameweek_id, manager_id, gw_transfers), by = c("gameweek_id", "manager_id"))

ft_mismatches <- player_transfers |> 
  filter(manual_gw_transfers != gw_transfers)

if (nrow(ft_mismatches) == 0) {
  cat("✓ All transfers match!\n")
} else {
  stop(
    paste0(
      "Transfer count mismatch in ", nrow(ft_mismatches), " gameweek(s):\n",
      paste(
        paste0("GW", ft_mismatches$gameweek_id, " - ", ft_mismatches$manager_id,
               " (transfers log: ", ft_mismatches$manual_gw_transfers,
               ", season: ", ft_mismatches$gw_transfers, ")"),
        collapse = "\n"
      )
    )
  )
}


## All Checks Passed -------------------------------------------------------------------------------------------
#| label: all_checks_passed
cat("✓ All validation checks passed — safe to write tables to DB and proceed to aggregation.\n")
