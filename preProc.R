library(RSQLite)  # install.packages("RSQLite")
library(data.table)

setwd("E:/Data.Science/NYCDSA/30_Coursework/Lectures/Week03/shiny_SQL_demo/shiny_demo/shiny_sqlite")

#csvpath = "path/to/csv"
csvpath = "../shiny_csv/flights14.csv"
dbname = "./flights.sqlite"
tblname = "flights"

## read csv
data <- fread(input = csvpath,
              sep = ",",
              header = TRUE)
## connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)
## write table
dbWriteTable(conn = conn,
             name = tblname,
             value = data)
## list tables
dbListTables(conn)
## disconnect
dbDisconnect(conn)
