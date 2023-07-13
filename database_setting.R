library(tidyverse)
library(RMySQL)
library(DBI)
library(dotenv)

# dotenv::load_dot_env()
# 
# db_user <- Sys.getenv("DB_USER")
# db_password <- Sys.getenv("DB_PASSWORD")
# db_host <- Sys.getenv("DB_HOST")
# db_dbname <- Sys.getenv("DB_DBNAME")
# db_port <- Sys.getenv("DB_PORT")
# 
# db <- dbConnect(
#   MySQL(),
#   user = db_user,
#   password = db_password,
#   host = db_host,
#   dbname = db_dbname,
#   port = as.numeric(db_port)
# )

connect_to_db <- function() {
  
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
  
  return(db)
}

# dbSendQuery(db, "CREATE TABLE keyword (
#             키워드 VARCHAR(20),
#             검색일 DATE,
#             트렌드 VARCHAR(20),
#             연관검색어 VARCHAR(20),
#             연관검색어_2 VARCHAR(20),
#             연관검색어_3 VARCHAR(20),
#             연관검색어_4 VARCHAR(20)
#             )")
# 
# dbDisconnect(db)
# dbSendQuery(db, "DROP TABLE keyword")

# dbGetQuery(db, "SHOW TABLES")
# dbGetQuery(db, "SHOW COLUMNS FROM keyword")

# print(result)

dbSendQuery(db, "DELETE FROM keyword WHERE 키워드 = '강동구 맛집';")

