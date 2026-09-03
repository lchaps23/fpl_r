library(DBI)
library(glue)

#' Writing to DB tables
#'
#' Inserts new rows, and updates existing rows where the key columns
#' already match — avoids duplicate errors when re-running the
#' pipeline over data that includes previously-written gameweeks.
#'
#' @param con         a DBI connection (from db_connection.R)
#' @param table_name  name of the target DB table, as a string
#' @param data        the tibble of new/updated rows to write
#' @param key_cols    character vector of the primary key column names
db_write <- function(con, table_name, data, key_cols) {
  
  staging_name <- paste0(table_name, "_staging")
  
  dbWriteTable(con, staging_name, data, temporary = TRUE, overwrite = TRUE)
  
  all_cols    <- names(data)
  update_cols <- setdiff(all_cols, key_cols)
  
  key_clause <- paste(key_cols, collapse = ", ")
  col_clause <- paste(all_cols, collapse = ", ")
  
  # Decide what to do on a key clash. If there are non-key columns,
  # update them from the new (EXCLUDED) row. If every column is part
  # of the key, there's nothing left to update — a clash means the
  # row already exists exactly as-is, so just skip it.
  if (length(update_cols) == 0) {
    conflict_action <- "DO NOTHING"
  } else {
    set_clause <- glue_collapse(
      glue("{update_cols} = EXCLUDED.{update_cols}"),
      sep = ",\n    "
    )
    conflict_action <- glue("DO UPDATE SET\n    {set_clause}")
  }
  
  query <- glue("
    INSERT INTO {table_name} ({col_clause})
    SELECT {col_clause} FROM {staging_name}
    ON CONFLICT ({key_clause})
    {conflict_action}
    RETURNING (xmax = 0) AS inserted
  ")
  
  result <- dbGetQuery(con, query)
  
  list(
    inserted = sum(result$inserted),
    updated  = sum(!result$inserted)
  )
}