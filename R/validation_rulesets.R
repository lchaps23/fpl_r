# Validation ruleset

# Season
season_rules <- validator(
  
  # -- Missingness -- 
  # Checking all columns from raw dataset as that is where manual data entry occurs. 
  # Other columns are calculated in cleaning.qmd
  !is.na(season_id),
  !is.na(date),
  !is.na(gameweek_id),
  !is.na(manager_id),
  !is.na(season_standing),
  !is.na(season_transfers),
  !is.na(overall_rank),
  !is.na(team_value),
  !is.na(gw_net),
  !is.na(total),
  
  
  # -- Range Checks --
  manager_range     = in_range(manager_id, min = 1, max = 10),
  gameweek_range  = in_range(gameweek_id,      min = 1,  max = 38),
  standing_range  = in_range(season_standing,  min = 1,  max = 8),
  transfers_range = in_range(season_transfers, min = 0,  max = 50),
  tv_range        = in_range(team_value,       min = 95, max = 110),
  
  
  # -- Chip Checks --
  valid_chip      = is.na(chip) | chip %in% c("BB1", "FH1", "WC1", "TC1", 
                                              "BB2", "FH2", "WC2", "TC2"),
  
  # -- Unique Checks --
  # Checking that there is a record for each manager x gameweek
  manager_gameweek = is_unique(season_id, gameweek_id, manager_id)
  
)



# Teams
teams_rules <- validator(
  
  # -- Missingness -- 
  # Checking all columns from raw dataset as that is where manual data entry occurs. 
  # Other columns are calculated in cleaning.qmd
  !is.na(season_id),
  !is.na(manager_id),
  !is.na(gameweek_id),
  !is.na(player_slot),
  !is.na(player),
  !is.na(points),
  !is.na(minutes),
  !is.na(manager_selected),
  !is.na(actual_start),
  !is.na(importance),
  
  # -- Range Checks --
  manager_range     = in_range(manager_id, min = 1, max = 10),
  gameweek_range    = in_range(gameweek_id, min = 1,  max = 38),
  player_slot_range = in_range(player_slot, min = 1,  max = 15),
  points_range      = in_range(points,      min = -5, max = 60),
  minutes_range     = in_range(minutes,     min = 0,  max = 180),
  
  # -- Unique Checks --
  # Checking that there is a record for 15 players per manager, per gameweek
  player_manager_gameweek = is_unique(season_id, manager_id, gameweek_id, player_slot)
  
)



# Transfers
transfers_rules <- validator(
  
  # -- Missingness -- 
  # Checking all columns from raw dataset as that is where manual data entry occurs. 
  # Other columns are calculated in cleaning.qmd
  !is.na(season_id),
  !is.na(gameweek_id),
  !is.na(manager_id),
  !is.na(player_out),
  !is.na(points_out),
  !is.na(player_in),
  !is.na(points_in),
  !is.na(points_diff),
  
  # -- Range Checks --
  manager_range    = in_range(manager_id, min = 1, max = 10),
  gameweek_range   = in_range(gameweek_id, min = 1,  max = 38),
  points_out_range = in_range(points_out,  min = -5, max = 60),
  points_in_range  = in_range(points_in,   min = -5, max = 60),
  
  # -- Unique Checks --
  # Checking that each transfer is only recorded once
  unique_transfer = is_unique(gameweek_id, manager, player_out, player_in)
  
)

