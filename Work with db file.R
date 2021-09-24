# Note: you need to install DBI package as well as RSQLite. 
library(DBI)
library(data.table)
# Then set up you work directory, in which your db file is located
setwd("C:/Users/xdli/Dropbox/UTD/2021/Fall/BA with R/R - Fall 2021-wb")

# store the db file name in a string variable
db_name <- 'wooldridge2.db'

# now estblish connection with target database.
con <- dbConnect(RSQLite::SQLite(),db_name)
dbListTables(con) # shows all the table in dababase "wooldridge2"

# This function pulls target table from a specific db. Note that the database connection was established before this function. 
# This function takes in argument of table name as string 'table_name'.
tb_pull <- function(tablename){
  dt <- dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(dbReadTable(con, paste0(tablename,'_labels')))
  return(dt)
}