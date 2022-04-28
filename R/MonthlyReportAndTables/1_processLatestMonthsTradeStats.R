#### Preparation ####

# Clear the environment
rm(list = ls())

# Load the required libraries
library(dplyr) # Manipulating data
library(stringr) # common string operations
library(tidyverse) # Manipulating data

# Note where VNSO code/data is on current computer
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "..")
setwd(repository) # Required for file.choose() function

# Load the general R functions
source(file.path(repository, "R", "functions.R"))

# Note the secure data path
secureDataFolder <- file.path(repository, "data", "secure")

# Note the open data path
openDataFolder <- file.path(repository, "data", "open")

# Note the outputs folder
outputsFolder <- file.path(repository, "outputs")

# Read in the raw trade data from secure folder of the repository 
tradeStatsFile <- file.path(secureDataFolder, "SEC_PROC_ASY_RawDataAndReferenceTables_31-03-22.csv")
tradeStats <- read.csv(tradeStatsFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) #replace blank cells with missing values-NA

# Get date from input file 
fileNameParts <- unlist(strsplit(tradeStatsFile, "_"))
fileDate <- unlist(strsplit(fileNameParts[length(fileNameParts)], "\\."))[1]

# Load the summary statistics for the historic IMPORTS and EXPORTS data
historicImportsSummaryStats <- read.csv(file.path(secureDataFolder, "imports_HS_summaryStats_02-10-20.csv"))
historicExportsSummaryStats <- read.csv(file.path(secureDataFolder, "exports_HS_summaryStats_02-10-20.csv"))

#### Clean and process the latest month's data ####

# Initial re-formatting of the data

# Add row ID column to track back to rows in raw unprocessed data
tradeStats$RawDataRowID <- 1:nrow(tradeStats)

# Remove the repeated header row from the trade statistics data
tradeStatsNoRepeatedHeader <- tradeStats[tradeStats$Office != "Office", ]

# Remove blank columns
emptyColumns <- apply(tradeStatsNoRepeatedHeader, MARGIN = 2, FUN = function(column){
  return(sum(is.na(column)) == length(column))
})
tradeStatsNoBlankCols <- tradeStatsNoRepeatedHeader[, emptyColumns == FALSE]

# Remove duplicated rows from the trade statistics data
duplicatedRows <- duplicated(tradeStatsNoBlankCols) 
tradeStatsNoDup <- tradeStatsNoBlankCols[duplicatedRows == FALSE, ]

# Convert the statistical value to numeric - note numbers formatted with commas in them and these need removed
tradeStatsNoDup$Stat..Value <- as.numeric(gsub(",", "", tradeStatsNoDup$Stat..Value))

# Convert excel figures to dates (choose)
tradeStatsNoDup$Reg..Date <- as.Date(tradeStatsNoDup$Reg..Date, format = "%d/%m/%Y")
#tradeStatsNoDup$Reg..Date <- as.Date(tradeStatsNoDup$Reg..Date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"), optional = FALSE)

# Covert SITC to character
tradeStatsNoDup$SITC <- as.character(tradeStatsNoDup$SITC)

# Pad HS.Code to 8 digits
tradeStatsNoDup$HS.Code <- sapply(tradeStatsNoDup$HS.Code, FUN=padWithZeros, "HS")

# Exclude banknotes and coins from exports
tradeStatsNoBanknotes <- tradeStatsNoDup[tradeStatsNoDup$HS.Code != "49070010", ]
tradeStatsNoCoins <- tradeStatsNoBanknotes[tradeStatsNoBanknotes$HS.Code != "71189000", ]

# Create subset for Export and Import commodities 
tradeStatsSubset <- tradeStatsNoCoins[tradeStatsNoCoins$Type %in% c("EX / 1","EX / 3", "IM / 4", "IM / 7", "PC / 4"), ]
tradeStatsCommodities <- tradeStatsSubset[tradeStatsSubset$CP4 %in% c(1000, 3071, 4000, 4071, 7100), ]

# Print progress
cat("Finished initial cleaning and processing of data.\n")

#### Explore the extent of missing data ####

# Count the number of missing values in each column (run numberMissing to see exact values)
numberMissing <- apply(tradeStatsCommodities, MARGIN=2,
                       FUN=function(columnValues){
                         return(sum(is.na(columnValues)))
                       })

# Convert the counts to a proportion
proportionMissing <- numberMissing / nrow(tradeStatsCommodities)

# Check for columns with high amounts of NA values
colsWithManyMissingValues <- names(proportionMissing)[proportionMissing > 0.1]
for(column in colsWithManyMissingValues){
  warning(paste0("Large amounts of missing data identified in \"", column, "\" column. View with: \n\tView(tradeStatsCommodities[is.na(tradeStatsCommodities[, \"",  column, "\"]), ])"))
}

# NAs present in any of above columns use, View(tradeStatsCommodities[is.na(tradeStatsCommodities$column_name), ])

# Print progress
cat("Finished exploring extent of missing data.\n")

#### Extract the SITC and HS sub-codes ####

# Extract the 6, 4, and 2 digit HS codes into separate columns
tradeStatsCommodities$HS.Code_6 <- extractCodeSubset(tradeStatsCommodities$HS.Code, nDigits=6)
tradeStatsCommodities$HS.Code_4 <- extractCodeSubset(tradeStatsCommodities$HS.Code, nDigits=4)
tradeStatsCommodities$HS.Code_2 <- extractCodeSubset(tradeStatsCommodities$HS.Code, nDigits=2)

# Extract the 3 and 1 digit SITC codes into separate columns
tradeStatsCommodities$SITC_3 <- extractCodeSubset(tradeStatsCommodities$SITC, nDigits=3)
tradeStatsCommodities$SITC_1 <- extractCodeSubset(tradeStatsCommodities$SITC, nDigits=1)

# If NAs present in any of the above columns, view with:
#    View(tradeStatsCommodities[is.na(tradeStatsCommodities$column_name), ]) # Replace column_name with name of column you're wanting to examine

# Print progress
cat("Added SITC and HS sub-codes into table.\n")

#### Extract the Year, Month, and Day as separate columns ####

# Create new columns for dates
tradeStatsCommodities$Year <- format(tradeStatsCommodities$Reg..Date, format= "%Y")
tradeStatsCommodities$Month <- format(tradeStatsCommodities$Reg..Date, format= "%B")
tradeStatsCommodities$Day <- format(tradeStatsCommodities$Reg..Date, format= "%d")

# Print progress
cat("Added separate date elements.\n")

#### Merge in classification tables ####

# Define classification tables and link columns
classificationTables <- data.frame(
  "file" = c(
    "OPN_FINAL_ASY_ModeOfTransportClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_HSCodeClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_SITCCodeClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_CountryDescriptionExportClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_PrincipleCommoditiesClassifications_31-01-20.csv",
    "OPN_FINAL_ASY_BECClassifications_31-01-20.csv"
  ),
  "link_column" = c("Office", "HS.Code_2", "SITC_1", "CO", "CE.CD", "HS.Code", "HS.Code_6")
)

# Merge in each of the classification tables
mergingOutputs <- mergeClassificationTablesIntoTradesData(tradeStats = tradeStatsCommodities,
                                                          classificationTables = classificationTables)
tradeStatsCommoditiesMergedWithClassifications <- mergingOutputs$tradeStatistics
missingClassificationCodeInfo <- mergingOutputs$missingCodeInfo


# Write missing codes table to file
write.csv(missingClassificationCodeInfo, file.path(outputsFolder, "OUT_PROC_ASY_missingClassifications_31-01-22.csv"))


# Print progress
cat("Finished merging in classification tables.\n")

#### Check if any statistical values fall outside of expected boundaries ####

# Check the commodity values against expected values based on historic data
commoditiesWithExpectations <- checkCommodityValues(tradeStatsCommoditiesMergedWithClassifications,  
                                                    historicImportsSummaryStats, historicExportsSummaryStats,
                                                    importCP4s=c(4000, 4071, 7100), exportCP4s=c(1000, 3071), useUnitValue=FALSE,
                                                    columnsOfInterest = c("HS.Code", "Type", "Reg..Date", 
                                                                          "CP4", "Itm..", "RawDataRowID"))

# Print progress
cat("Finished checking whether commodity values fall outside of expectations based on historic data.\n")

#### Finish ####

# Make copy of latest month's processed data
processedTradeStats <- tradeStatsCommoditiesMergedWithClassifications

write.csv(processedTradeStats, "processedTradeStats.csv")

# Create new column catgorising export, re-export and import
tradeMerchandiseFile <- file.path(openDataFolder, "OPN_FINAL_ASY_MerchandiseTradeClassifications_15-03-22.csv")
tradeMerchandiseClass <- read.csv(tradeMerchandiseFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) #replace blank cells with missing values-NA
tradeMerchandiseMerged <- merge(processedTradeStats, tradeMerchandiseClass, by="CP4", all.x=TRUE)

# Append processed data to historical data
historicalTradeStatsFile <- file.path(secureDataFolder, "exports-imports_historical_14.03.22.csv")
historicalTradeStats <- read.csv(historicalTradeStatsFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) 

historicalTradeStatsAdapted<- historicalTradeStats %>% #convert all data to same type for data to be appended
  mutate_all(as.character)
tradeMerchandiseMergedAdapted<- tradeMerchandiseMerged %>%
  mutate_all(as.character)

historicalDataUpdated<- bind_rows(historicalTradeStatsAdapted, tradeMerchandiseMergedAdapted, id= NULL)

historicalDataFile <- file.path(secureDataFolder, paste("OUT_PROC_ASY_ProcessedHistoricalData_",fileDate,".csv"))
write.csv(historicalDataUpdated, historicalDataFile)

# Create csv of last months processed data
outputDataFile <- file.path(
  secureDataFolder, 
  paste("OUT_PROC_ASY_ProcessedRawData_",fileDate,".csv")
)

write.csv(processedTradeStats, outputDataFile)
-
# Note progress final

cat(paste0("Finished processing and cleaning latest month's data into:\n\t", outputDataFile, "\n"))
