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
tradeStatsFile <- file.path(secureDataFolder, "SEC_PROC_ASY_RawDataAndReferenceTables_30-09-21.csv")
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
#tradeStatsNoDup$Reg..Date <- as.Date(tradeStatsNoDup$Reg..Date, format = "%d/%m/%Y")
tradeStatsNoDup$Reg..Date <- as.Date(tradeStatsNoDup$Reg..Date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"), optional = FALSE)

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
classification_tables <- data.frame(
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
                                                          classificationTables = classification_tables)
tradeStatsCommoditiesMergedWithClassifications <- mergingOutputs$tradeStatistics
missingClassificationCodeInfo <- mergingOutputs$missingCodeInfo

# Refer to your correlation tables
write.csv(missingClassificationCodeInfo, "missingClassifications.csv")


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
write.csv(processedTradeStats, file.path(secureDataFolder, "OUT_PROC_ASY_ProcessedRawData_30-09-21.csv"))

# Note progress final

cat("Finished processing and cleaning latest month's data.\n")



importsCombinedFile <- file.path("importsCombined.csv")
importsHistoricalFile <- file.path("imports_history_summary.csv")

importsCombined <- read.csv(importsCombinedFile, header=TRUE, na.strings=c("","NA", "NULL", "null"))
importsHistorical <- read.csv(importsHistoricalFile, header=TRUE, na.strings=c("","NA", "NULL", "null"))

importsHistorical$Office<- "NA"
importsHistorical$Reg.Ref<- "NA"


importsHistorical<- importsHistorical %>% 
  rename(
    CP4 = Procedure,
    HS.Code= HS,
    Goods.Comm..Dsc.1= Description,
    CO= CTY_Origin,
    Wgt..Net= Weight,
    Pkg..type= Pkg_Type,
    Supp.Qty= Supp_Qty,
    Stat..Value= Value)

importsHistoricalUpdated <- importsHistorical[ -c(1,13,15) ]

importsHistoricalUpdated$Type<- "NA"
importsHistoricalUpdated$CP3<- "NA"
importsHistoricalUpdated$Declarant<- "NA"
importsHistoricalUpdated$Con_cod<- "NA"
importsHistoricalUpdated$Con...Exp.name<- "NA"
importsHistoricalUpdated$Shipper...Buyer.name<- "NA"
importsHistoricalUpdated$Itm..<- "NA"
importsHistoricalUpdated$PRF<- "NA"
importsHistoricalUpdated$Goods.Comm..Dsc.<- "NA"
importsHistoricalUpdated$CE.CD<- "NA"
importsHistoricalUpdated$Wgt..Gross<- "NA"
importsHistoricalUpdated$Pkg..<- "NA"
importsHistoricalUpdated$Supp<- "NA"
importsHistoricalUpdated$TOD<- "NA"
importsHistoricalUpdated$TOD<- "NA"
importsHistoricalUpdated$IMD<- "NA"
importsHistoricalUpdated$VAT<- "NA"
importsHistoricalUpdated$IEX<- "NA"
importsHistoricalUpdated$DEX<- "NA"
importsHistoricalUpdated$EXD<- "NA"
importsHistoricalUpdated$OED<- "NA"
importsHistoricalUpdated$Day<- "NA"

newImportsHistorical<- importsHistoricalUpdated[,c(13,14,1,15,4,16,17,18,19,20,21,5,6,22,7,23,8,24,25,9,26,10,11,27,28,12,29,30,31,32,33,34,2,3,35)]

importsTotal<- rbind(newImportsHistorical, importsCombined)

importsTotalRecoded<- importsTotal %>% mutate(sex=recode(sex, 
                         `1`="Male",
                         `2`="Female"))


write.csv(importsTotal, "totalimportsCombined.csv")



combinedFile <- file.path("combinedExports2021.csv")
Exports2021 <- read.csv(combinedFile, header=TRUE, na.strings=c("","NA", "NULL", "null"))

Exports2021$Reg..Date <- as.Date(Exports2021$Reg..Date, format = "%d/%m/%Y")

Exports2021$Year <- format(Exports2021$Reg..Date, format= "%Y")
Exports2021$Month <- format(Exports2021$Reg..Date, format= "%B")
Exports2021$Day <- format(Exports2021$Reg..Date, format= "%d")
write.csv(Exports2021, "newcombinedExports2021.csv")
