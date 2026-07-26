# Free Transfer Calculations
calc_ft <- function(df, afcon_trf = FALSE) {
  
  n            <- nrow(df)
  gw_transfers <- c(df$season_transfers[1], diff(df$season_transfers))
  is_wc_or_fh  <- !is.na(df$chip) & grepl("WC|FH", df$chip)
  
  ft_banked  <- numeric(n)
  ft_used    <- numeric(n)
  extra_trf  <- numeric(n)
  hit_points <- numeric(n)
  
  for (i in seq_len(n)) {
    
    made  <- gw_transfers[i]
    wc_fh <- is_wc_or_fh[i]
    
    if (i == 1) {
      banked <- 0
      
    } else {
      carried <- if (is_wc_or_fh[i - 1]) ft_banked[i - 1]
      else                     ft_banked[i - 1] - ft_used[i - 1]
      
      banked <- if (wc_fh) carried
      else       min(carried + 1, 5)
      
      if (df$gameweek_id[i] == 16 && afcon_trf) banked <- 5
    }
    
    ft_banked[i] <- banked
    
    if (wc_fh) {
      ft_used[i]    <- 0
      extra_trf[i]  <- 0
      hit_points[i] <- 0
      
    } else {
      ft_used[i]    <- min(made, banked)
      extra_trf[i]  <- max(made - banked, 0)
      hit_points[i] <- extra_trf[i] * -4
    }
  }
  
  df$gw_transfers <- gw_transfers
  df$ft_remaining <- pmax(ft_banked - ft_used, 0)
  df$ft_used      <- ft_used
  df$extra_trf    <- extra_trf
  df$hit_points   <- hit_points
  df
  
}
