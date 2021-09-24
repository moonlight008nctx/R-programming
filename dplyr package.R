# Data manipulation with dplyr package
# Four verbs
## select(): i.e., select columns of interest
## filter(): extract particular values 
## arrange(): sort data based on one or more variables
## mutate(): add new variables or change existing variables 
## count(): aggregate data in counted number.

library(dplyr)

# our data source is the lawsch85 table from wooldridge2.db
library(DBI)
library(data.table)
setwd("C:/Users/xdli/Dropbox/UTD/2021/Fall/BA with R/R - Fall 2021-wb")
db_name <- 'wooldridge2.db'
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

# pull the table using the function tb_pull
lawsch85 <- tb_pull("lawsch85")
lawsch85

# select columns of interest
lawsch_tb <- lawsch85 %>%
  select(salary, LSAT,GPA, libvol,cost,rank)

# Note %>% is a pipe operator. This operator will forward a value, or the result of an expression, into the next function call/expression. 

# this code sort the data based on descending order of ranking. 
lawsch_tb %>%
  arrange(desc(rank))

# this code sort the data based on descending order of ranking, only for those records that has NA value as LSAT 
lawsch_tb %>%
  arrange(desc(rank)) %>%
  filter(is.na(LSAT))

# the following code add a new column, which represent salary in K unit. 
lawsch_tb %>% 
  mutate(salary_K = salary/1000)

# in the count() code below without specifying criteria for count(), the output result is the same as total number of rows. 
lawsch_tb %>%
  count()
nrow(lawsch_tb)

# transform rank into a factor/catgory of 5 levels, using cut() 
lawsch_tb$rank_cg <- cut(lawsch_tb$rank,5,labels=FALSE)
# then we can count the number of records in each of the rank_cg, when using "sort=TRUE", the output is sorted based on n in a descending order.
lawsch_tb %>%
  count(rank_cg, sort=TRUE)


