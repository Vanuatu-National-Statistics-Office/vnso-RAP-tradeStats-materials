#### Preparation ####

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
tradeStatsFile <- file.path(secureDataFolder, "SEC_PROC_ASY_RawDataAndReferenceTables_31-01-20.csv")
tradeStats <- read.csv(tradeStatsFile, header=TRUE, na.strings=c("","NA", "NULL", "null")) #replace blank cells with missing values-NA
# tradeStats <- read.csv(tradeStatsFile, header=TRUE, na.strings=c("","NA", "NULL", "null"), skip = 1)

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

# Convert excel figures to dates
tradeStatsNoDup$Reg..Date <- as.Date(tradeStatsNoDup$Reg..Date, format = "%d/%m/%Y")

# Convert SITC to character
tradeStatsNoDup$SITC <- sapply(tradeStatsNoDup$SITC, FUN=padWithZeros, "SITC")

# Convert HS.Code to character
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
numberMissing

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

## MODE OF TRANSPORT ##

# Merge Mode of Transport classifications with cleaned data
tradeStatsFileMergeTransport <- file.path(openDataFolder, "OPN_FINAL_ASY_ModeOfTransportClassifications_31-01-20.csv")
modeOfTransport <- read.csv(tradeStatsFileMergeTransport)
colnames(modeOfTransport)[1] <- "Office"
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommodities, modeOfTransport, by="Office", all.x=TRUE)

missingValuesOffice <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  modeOfTransport,
  tradeStatsColumn = "Office",
  classificationTableName = "Mode of transport")

## HARMONISED SYSTEM (HS) CODES ##

# Merge Harmonised System (HS) Code classifications with cleaned data
tradeStatsFileMergeHSCode <- file.path(openDataFolder, "OPN_FINAL_ASY_HSCodeClassifications_31-01-20.csv") 
hsDescription <- read.csv(tradeStatsFileMergeHSCode)
colnames(hsDescription)[1] <- "HS.Code_2"
hsDescription$HS.Code_2 <- sapply(hsDescription$HS.Code_2, FUN=padWithZeros, "HS", 2)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, hsDescription, by="HS.Code_2", all.x=TRUE)

missingValuesHSCode_2 <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  hsDescription,
  tradeStatsColumn = "HS.Code_2",
  classificationTableName = "HS descriptions")

## STANDARD INTERNATIONAL TRADE CLASSIFICATION (SITC) CODES ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeSITCCode <- file.path(openDataFolder, "OPN_FINAL_ASY_SITCCodeClassifications_31-01-20.csv") 
sitcDescription <- read.csv(tradeStatsFileMergeSITCCode)
colnames(sitcDescription) <- c("SITC_1", "SITC.description")
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, sitcDescription, by="SITC_1", all.x=TRUE)

missingValuesSITC_1 <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  sitcDescription,
  tradeStatsColumn = "SITC_1",
  classificationTableName = "SITC descriptions")

## COUNTRY DESCRIPTIONS OF IMPORTS ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeCountryDesImports <- file.path(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.csv")
countryDescriptionImports <- read.csv(tradeStatsFileMergeCountryDesImports)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, countryDescriptionImports, by="CO", all.x=TRUE)

missingValuesImportCountry <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  countryDescriptionImports,
  tradeStatsColumn = "CO",
  classificationTableName = "Import country")

## COUNTRY DESCRIPTIONS OF EXPORTS ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeCountryDesExports <- file.path(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionExportClassifications_31-01-20.csv") 
countryDescriptionExports <- read.csv(tradeStatsFileMergeCountryDesExports)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, countryDescriptionExports, by="CE.CD", all.x=TRUE)

missingValuesExportCountry <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  countryDescriptionExports,
  tradeStatsColumn = "CE.CD",
  classificationTableName = "Export country")

## PRINCIPLE COMMODITIES ##

# Merge Principle Commodity Classifications of Exports and Imports with cleaned data
tradeStatsFileMergePrincipleCommdityClass <- file.path(openDataFolder, "OPN_FINAL_ASY_PrincipleCommoditiesClassifications_31-01-20.csv") 
principleCommodityClassification <- read.csv(tradeStatsFileMergePrincipleCommdityClass)
colnames(principleCommodityClassification)[1] <- "HS.Code"
hsDescription$HS.Code <- sapply(hsDescription$HS.Code, FUN=padWithZeros, "HS")
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, principleCommodityClassification, by="HS.Code", all.x=TRUE)

missingValuesPrincipleCommodities <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  principleCommodityClassification,
  tradeStatsColumn = "HS.Code",
  classificationTableName = "Principle Commodities")

## BROAD ECONOMIC CATEGORIES (BEC) ##

# Merge Broad Economic Categories (BEC) classifications with cleaned data
tradeStatsFileMergeBECDescriptions <- file.path(openDataFolder, "OPN_FINAL_ASY_BECClassifications_31-01-20.csv")
becDescriptions <- read.csv(tradeStatsFileMergeBECDescriptions)
becDescriptions$HS.Code_6<- sapply(becDescriptions$HS.Code_6, FUN=padWithZeros, "HS", 6)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, becDescriptions, by="HS.Code_6", all.x=TRUE)

missingValuesBEC <- checkMergingColumnsForClassificationTables(
  tradeStatsCommoditiesMergedWithClassifications,
  becDescriptions,
  tradeStatsColumn = "HS.Code_6",
  classificationTableName = "BEC descriptions")

## Check number of rows with NA values present
prfColumn <- which(colnames(tradeStatsCommoditiesMergedWithClassifications) == "PRF")
rowNACounts <- rowSums(is.na(tradeStatsCommoditiesMergedWithClassifications[, -prfColumn]))
rowsWithNAValues <- tradeStatsCommoditiesMergedWithClassifications[rowNACounts > 0, ]
if(nrow(rowsWithNAValues) > 0){
  warning(paste0("View the ", nrow(rowsWithNAValues), " rows of the trade statistics data with missing values with: View(rowsWithNAValues)"))
}

# Print progress
cat("Finished merging in classification tables.\n")

#### Check if any statistical values fall outside of expected boundaries ####

# Load the summary statistics for the historic IMPORTS and EXPORTS data
historicImportsSummaryStats <- read.csv(file.path(secureDataFolder, "imports_HS_summaryStats_02-10-20.csv"))
historicExportsSummaryStats <- read.csv(file.path(secureDataFolder, "exports_HS_summaryStats_02-10-20.csv"))

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


# Create csv of last months processed data
write.csv(processedTradeStats, file.path(secureDataFolder, "OUT_PROC_ASY_ProcessedRawData_31-01-20.csv"))

# Note progress

cat("Finished processing and cleaning latest month's data.\n")
