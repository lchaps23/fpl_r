library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "postgres",
  host     = "aws-1-eu-west-2.pooler.supabase.com",
  port     = 5432,
  user     = "postgres.xrdkgxwkyqpsosjdvebb",
  password = Sys.getenv("SUPABASE_PW")
)