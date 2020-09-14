#### Preparation ####

# Load libraries
library(RMySQL)

# Open a connection to a MySQL database
connection <- dbConnect(MySQL(), 
                        user='JosephCrispell', 
                        password=readline(prompt="Enter password: "), # Doing this as password never stored in easily accessible format now
                        dbname='vnso',
                        host = 'localhost')

# List the tables available in the sakila database
dbListTables(connection)

#### Explore the historic data ####

# Get the historic data
historic <- dbGetQuery(conn=connection, statement="SELECT * FROM historical_export_99_19")

# Save the data as a standard csv
write.table(historic, file=file.path("..", "data", "secure", "tradeStats_historic_14-09-20.csv"),
            sep=",", row.names=FALSE, quote=TRUE)
