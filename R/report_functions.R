# These functions are only to be used for producing season scorecards.
# They do not form part of the _targets analysis pipeline and are therefore
# separated in to their own folder.

# Help function to add suffixes to ranks
# E.g. 1 -> 1st; 2 -> 2nd, etc.
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



# Help function to apply ranking to a given variable
# Pass through a dataframe with a given variable to rank
# high_to_low = TRUE will rank the highest value of the variable as 1st
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



# ---- Season Overview Functions ---- #
# Function to generate the best and worst values for a given metric.
# Attaches the GW that the value occurred in.
best_worst_metric <- function(df, metric_col, label_col, prefix = "GW", rounding = 1) {
  
  if (rlang::as_label(enquo(metric_col)) == "season_standing") {
    
    df |> 
      summarise(
        best_value     = as.integer(min({{ metric_col }})),
        best_gw        = {{ label_col }}[which.min({{ metric_col }})],
        worst_value    = as.integer(max({{ metric_col }})),
        worst_gw       = {{ label_col }}[which.max({{ metric_col }})]
      ) |> 
      mutate(
        best_value  = rank_suffix(best_value),
        best_label  = paste0(best_value, " (", prefix, best_gw, ")"),
        worst_value = rank_suffix(worst_value),
        worst_label = paste0(worst_value, " (", prefix, worst_gw, ")")
      )
    
  } else {
    
    df |> 
      summarise(
        best_value     = max({{ metric_col }}),
        best_gw        = {{ label_col }}[which.max({{ metric_col }})],
        worst_value    = min({{ metric_col }}),
        worst_gw       = {{ label_col }}[which.min({{ metric_col }})]
      ) |> 
      mutate(
        best_label  = paste0(scales::comma(best_value, accuracy = rounding), " (", prefix, best_gw, ")"),
        worst_label = paste0(scales::comma(worst_value, accuracy = rounding), " (", prefix, worst_gw, ")")
      )
    
  }
}
