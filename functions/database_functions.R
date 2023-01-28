
# Oliver Eaton
# Begun: 2021-05-27

# File to host functions that interact with the database

# Init message ------------------------------------------------------------

print("Custom database functions...")
print("")


# Environ -----------------------------------------------------------------

library(RMySQL)

# Connection to MySQL database
sql_con = dbConnect(MySQL(), user = "oli", dbname = "political_twitter")


# Read Function -----------------------------------------------------------

read_table <- function(table){
  dbReadTable(sql_con, table) %>% 
    mutate(across(contains(c("created_at", "date")), as.POSIXct))
}
  

# Write Function ----------------------------------------------------------

write_table <- function(table){
  dbWriteTable(
    sql_con
    , deparse(substitute(table))
    , table
    , row.names = FALSE
    , overwrite = TRUE
  )
}

