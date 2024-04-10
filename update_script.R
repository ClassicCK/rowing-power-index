library(RSQLite)
library(DBI)
library(dplyr)

# Assuming db is your database connection
db <- dbConnect(SQLite(), dbname = "rpi.db")

# Run the Elo rating update process
RPI(db)

# Close the database connection
dbDisconnect(db)
