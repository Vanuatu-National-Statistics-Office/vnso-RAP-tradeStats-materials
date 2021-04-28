# Clear the environment
rm(list = ls())

# Load the required libraries
library(dplyr) # Manipulating data

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Load the general R functions
source(file.path(repository, "R", "functions.R"))

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Read in the raw trade data from secure folder of the repository 
# Note that spaces in column names have been automatically replaced with "."s
tradeStatsFile <- file.path(secureDataFolder, "SEC_PROC_ASY_RawData_31-01-20.csv")
tradeStats <- read.csv(tradeStatsFile)

# Remove the repeated header row from the trade statistics data
tradeStats <- tradeStats[tradeStats$Office != "Office", ]

# Note the date column as dates
tradeStats$`Reg..Date` <- as.Date(tradeStats$`Reg..Date`, format = "%d/%m/%Y")

# Remove empty column
emptyColumns <- apply(tradeStats, MARGIN = 2,
                      FUN = function(column){
                        return(sum(is.na(column)) == length(column))
                      })
tradeStats <- tradeStats[, emptyColumns == FALSE]
