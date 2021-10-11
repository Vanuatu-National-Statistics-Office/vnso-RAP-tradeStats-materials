# Clear the environment
rm(list = ls())

# Load the required libraries
library(dplyr) # Manipulating data
library(stringr) # common string operations

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Load the general R functions
source(file.path(repository, "R", "functions.R"))

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

# Read in the raw trade data from secure folder of the repository 
tradeStatsFile <- file.path(secureDataFolder, "imports_HS_summaryStats_02-10-20.csv")
tradeStats <- read.csv(tradeStatsFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) #replace blank cells with missing values-NA
tradeStats20162018<- tradeStats[tradeStats$Year %in% c("2016", "2017", "2018"), ]

# Merge country classifications
tradeStatsFileMergeCountryDesImports <- file.path(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.csv")
countryDescriptionImports <- read.csv(tradeStatsFileMergeCountryDesImports)
tradeStats20162018 <- merge(tradeStats20162018, countryDescriptionImports, by="CTY_Origin", all.x=TRUE)

# Convert HS.Code to character
tradeStats20162018$HS <- sapply(tradeStats20162018$HS, FUN=padWithZeros, "HS")

# Extract the 6, 4, and 2 digit HS codes into separate columns
tradeStats20162018$HS.Code_6 <- extractCodeSubset(tradeStats20162018$HS, nDigits=6)
tradeStats20162018$HS.Code_4 <- extractCodeSubset(tradeStats20162018$HS, nDigits=4)

# Create subset for eggs, poultry and meat produces
eggsTradeStats<- tradeStats20162018[tradeStats20162018$HS == "04071100", ]
poultryTradeStats<- tradeStats20162018[tradeStats20162018$HS.Code_4 == "0207", ]
meatTradeStats<- tradeStats20162018[tradeStats20162018$HS.Code_4 %in% c("1602", "1604", "1605"), ]

eggsAnnualValue <- eggsTradeStats %>%
  group_by(Year, IMPORT.COUNTRY) %>%
  summarise(Total.VT = sum(Value),
            Total.Weight = sum(Weight))

poultryAnnualValue <- poultryTradeStats %>%
  group_by(Year, IMPORT.COUNTRY) %>%
  summarise(Total.VT = sum(Value),
            Total.Weight = sum(Weight))

meatAnnualValue <- meatTradeStats %>%
  group_by(Year, IMPORT.COUNTRY) %>%
  summarise(Total.VT = sum(Value),
            Total.Weight = sum(Weight))

# Create csv of last months processed data
write.xlsx(eggsAnnualValue, file.path(secureDataFolder, "eggsAnnualValue.xlsx"))
write.xlsx(poultryAnnualValue, file.path(secureDataFolder, "poultryAnnualValue.xlsx"))
write.xlsx(meatAnnualValue, file.path(secureDataFolder, "meatAnnualValue.xlsx"))

