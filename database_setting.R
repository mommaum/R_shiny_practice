library(tidyverse)
library(RMySQL)
library(DBI)
library(dotenv)

dotenv::load_dot_env()

db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_host <- Sys.getenv("DB_HOST")
db_dbname <- Sys.getenv("DB_DBNAME")
db_port <- Sys.getenv("DB_PORT")

db <- dbConnect(
  MySQL(),
  user = db_user,
  password = db_password,
  host = db_host,
  dbname = db_dbname,
  port = as.numeric(db_port)
)

dbSendQuery(db, "CREATE TABLE keyword(
            ID int,
            keyword char,
            keyword_2 char,
            start_date date,
            end_date date,
            cor double
            )")

dbListTables(db) %>% print()

result <- dbGetQuery(
  db,
  "select * from keyword;"
)

print(result)
