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

# Exclude banknotes from exports
tradeStatsNoBanknotes <- tradeStatsNoDup[tradeStatsNoDup$HS.Code != "49070010", ]

# Create subset for Export and Import commodities 
tradeStatsSubset <- tradeStatsNoBanknotes[tradeStatsNoBanknotes$Type %in% c("EX / 1","EX / 3", "IM / 4", "IM / 7", "PC / 4"), ]
tradeStatsCommodities <- tradeStatsSubset[tradeStatsSubset$CP4 %in% c(1000, 3071, 4000, 4071, 7100), ]

# Print progress
cat("Finished initial cleaning and processing of data.\n")

#### Explore the extent of missing data ####

# Count the number of missing values in each column
numberMissing <- apply(tradeStatsCommodities, MARGIN=2,
                       FUN=function(columnValues){
                         return(sum(is.na(columnValues)))
                       })

# Convert the counts to a proportion
proportionMissing <- numberMissing / nrow(tradeStatsCommodities)

# Set the plotting margins
par(mar=c(10, 4, 4, 1))

# Check for columns with high amounts of NA values
colsWithManyMissingValues <- names(proportionMissing)[proportionMissing > 0.1]
for(column in colsWithManyMissingValues){
  warning(paste0("Large amounts of missing data identified in \"", column, "\" column. Use View(tradeStatsCommodities[is.na(tradeStatsCommodities[, \"",  column, "\"]) to view."))
}

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
rowsWithoutValidModeOfTransport <- which(tradeStatsCommodities$Office %in% modeOfTransport$Office == FALSE)
if(length(rowsWithoutValidModeOfTransport) > 0){
  warning("Some codes in the \"Office\" column of the trades statistics table are missing in the mode of transport classifications table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidModeOfTransport, \"Office\"])")
}

## HARMONISED SYSTEM (HS) CODES ##

# Merge Harmonised System (HS) Code classifications with cleaned data
tradeStatsFileMergeHSCode <- file.path(openDataFolder, "OPN_FINAL_ASY_HSCodeClassifications_31-01-20.csv") 
hsDescription <- read.csv(tradeStatsFileMergeHSCode)
colnames(hsDescription)[1] <- "HS.Code_2"
hsDescription$HS.Code_2 <- sapply(hsDescription$HS.Code_2, FUN=padWithZeros, "HS", 2)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, hsDescription, by="HS.Code_2", all.x=TRUE)
rowsWithoutValidHS2Code <- which(tradeStatsCommoditiesMergedWithClassifications$HS.Code_2 %in% hsDescription$HS.Code_2 == FALSE)
if(length(rowsWithoutValidHS2Code) > 0){
  warning("Some codes in the \"HS.Code_2\" column of the trades statistics table are missing in the HS 2 classifications table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidHS2Code, \"HS.Code_2\"])")
}

## STANDARD INTERNATIONAL TRADE CLASSIFICATION (SITC) CODES ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeSITCCode <- file.path(openDataFolder, "OPN_FINAL_ASY_SITCCodeClassifications_31-01-20.csv") 
sitcDescription <- read.csv(tradeStatsFileMergeSITCCode)
colnames(sitcDescription) <- c("SITC_1", "SITC.description")
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, sitcDescription, by="SITC_1", all.x=TRUE)
rowsWithoutValidSITC1 <- which(tradeStatsCommoditiesMergedWithClassifications$SITC_1 %in% sitcDescription$SITC_1 == FALSE)
if(length(rowsWithoutValidSITC1) > 0){
  warning(length(rowsWithoutValidSITC1), " codes in the \"SITC_1\" column of the trades statistics table are missing in the SITC classifications table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidSITC1, \"SITC_1\"])")
}

## COUNTRY DESCRIPTIONS OF IMPORTS ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeCountryDesImports <- file.path(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.csv")
countryDescriptionImports <- read.csv(tradeStatsFileMergeCountryDesImports)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, countryDescriptionImports, by="CO", all.x=TRUE)
rowsWithoutValidImportCountryCode <- which(tradeStatsCommoditiesMergedWithClassifications$CO %in% countryDescriptionImports$CO == FALSE)
if(length(rowsWithoutValidImportCountryCode) > 0){
  warning(length(rowsWithoutValidImportCountryCode), " codes in the \"CO\" column of the trades statistics table are missing in the import country classifications table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidImportCountryCode, \"CO\"])")
}


## COUNTRY DESCRIPTIONS OF EXPORTS ##

# Merge Standard International Trade Classification (SITC) Code classifications with cleaned data
tradeStatsFileMergeCountryDesExports <- file.path(openDataFolder, "OPN_FINAL_ASY_CountryDescriptionExportClassifications_31-01-20.csv") 
countryDescriptionExports <- read.csv(tradeStatsFileMergeCountryDesExports)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, countryDescriptionExports, by="CE.CD", all.x=TRUE)
rowsWithoutValidExportCountryCode <- which(tradeStatsCommoditiesMergedWithClassifications$CO %in% countryDescriptionExports$CE.CD == FALSE)
if(length(rowsWithoutValidExportCountryCode) > 0){
  warning(length(rowsWithoutValidExportCountryCode), " codes in the \"CE.CD\" column of the trades statistics table are missing in the export country classifications table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidExportCountryCode, \"CE.CD\"])")
}

## PRINCIPLE COMMODITIES ##

# Merge Principle Commodity Classifications of Exports and Imports with cleaned data
tradeStatsFileMergePrincipleCommdityClass <- file.path(openDataFolder, "OPN_FINAL_ASY_PrincipleCommoditiesClassifications_31-01-20.csv") 
principleCommodityClassification <- read.csv(tradeStatsFileMergePrincipleCommdityClass)
colnames(principleCommodityClassification)[1] <- "HS.Code"
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, principleCommodityClassification, by="HS.Code", all.x=TRUE)
rowsNotInPrincipleCommoditiesTable <- which(tradeStatsCommoditiesMergedWithClassifications$HS.Code %in% principleCommodityClassification$HS.Code == FALSE)
if(length(rowsNotInPrincipleCommoditiesTable) > 0){
  warning(length(rowsNotInPrincipleCommoditiesTable), " codes in the \"HS.Code\" column of the trades statistics table are missing in the principle commodities classification table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsNotInPrincipleCommoditiesTable, \"HS.Code\"])")
}

## BROAD ECONOMIC CATEGORIES (BEC) ##

# Merge Broad Economic Categories (BEC) classifications with cleaned data
tradeStatsFileMergeBECDescriptions <- file.path(openDataFolder, "OPN_FINAL_ASY_BECClassifications_31-01-20.csv")
becDescriptions <- read.csv(tradeStatsFileMergeBECDescriptions)
tradeStatsCommoditiesMergedWithClassifications <- merge(tradeStatsCommoditiesMergedWithClassifications, becDescriptions, by="HS.Code_6", all.x=TRUE)
rowsWithoutValidHS6 <- which(tradeStatsCommoditiesMergedWithClassifications$HS.Code_6 %in% becDescriptions$HS.Code_6 == FALSE)
if(length(rowsWithoutValidHS6) > 0){
  warning(length(rowsWithoutValidHS6), " codes in the \"HS.Code_6\" column of the trades statistics table are missing in the BEC classification table. Examine with View(tradeStatsCommoditiesMergedWithClassifications[rowsWithoutValidHS6, \"HS.Code_6\"])")
}

## Check number of rows with NA values present
prfColumn <- which(colnames(tradeStatsCommoditiesMergedWithClassifications) == "PRF")
rowNACounts <- rowSums(is.na(tradeStatsCommoditiesMergedWithClassifications[, -prfColumn]))
rowsWithNAValues <- tradeStatsCommoditiesMergedWithClassifications[rowNACounts > 0, ]
if(nrows(rowsWithNAValues) > 0){
  warning("View the rows of the trade statistics data with missing values with: View(tradeStatsCommoditiesMergedWithClassifications[rowsWithNAValues, ]")
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
                                                                          "CP4", "Itm.."))

# Print progress
cat("Finished checking whether commodity values fall outside of expectations based on historic data.\n")

#### Finish ####

# Make copy of latest month's processed data
processedTradeStats <- tradeStatsCommoditiesMergedWithClassifications

# Create csv of last months processed data
write.csv(processedTradeStats, "OUT_PROC_ASY_ProcessedRawData_31-01-20.csv")

# Note progress

cat("Finished processing and cleaning latest month's data.\n")