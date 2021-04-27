#### Preparation ####

# Check that 1_processLatestMonthsTradeStats.R has already been run
if(exists("repository") == FALSE || exists("secureDataFolder") == FALSE || 
   exists("openDataFolder") == FALSE || exists("processedTradeStats") == FALSE){
  stop("Necessary variables have not been created. Please ensure that all code in \"1_processLatestMonthsTradeStats.R\" has been executed successfully.")
}

# Note the outputs folder
outputsFolder <- file.path(repository, "outputs")

# Note current year and month of data
date <- max(processedTradeStats$Reg..Date, na.rm=TRUE)
month <- format(date, "%B")
year <- format(date, "%Y")

# Note the excel workbook containing the final formatted tables
finalWorkbookFileName <- file.path(outputsFolder, "SEC_FINAL_MAN_FinalTradeStatisticsTables_30-11-19_WORKING.xlsx")

# Load the excel file
finalWorkbook <- openxlsx::loadWorkbook(finalWorkbookFileName)

# Print progress
cat("Finished loading excel file with formatted tables.\n")

#### Table 1: Balance of Trade ####

## Getting latest statistics ##

# Calculate the balance of trade statistics - note that this remove a bit of redundancy
exports <- sum(processedTradeStats[processedTradeStats$CP4 == 1000, "Stat..Value"], na.rm=TRUE) # Added na.rm in case NAs introduced at later date
reExports <- sum(processedTradeStats[processedTradeStats$CP4 == 3071, "Stat..Value"], na.rm=TRUE)
totalExports <- exports + reExports
imports <- sum(processedTradeStats[processedTradeStats$CP4 %in% c(4000, 4071, 7100), "Stat..Value"], na.rm=TRUE)
balance <- totalExports - imports

# Insert values into table
tradeBalance <- data.frame("Export"=exports, "Re-Export"=reExports, "Total Export"=totalExports, "Imports CIF"=imports, "Trade Balance"=balance)

## Formatting the table ##

# Extract the balance of trade data and extract sub tables
balanceOfTradeSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="1_BOT", nColumns=7, startRow=5)

# Update the sub tables for the balance of trade data
balanceOfTradeSubTables <- updateSubTablesByTime(balanceOfTradeSubTables, month, year, tradeBalance/1000000)

# Editing the final formatted tables
if(is.null(balanceOfTradeSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="1_BOT", subTables=balanceOfTradeSubTables, nRowsInHeader=5,
                                         loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 1: Balance of Trade.\n")

#### Table 2: Import by HS Code ####

## Getting latest statistics ##
# Define the CP4 codes need for table (Imports)
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "HS.Code_2"
columnCategories <- list(
  "Live animals: animal products"=c("01", "02", "03", "04", "05"),
  "Vegetable products"=c("06", "07", "08", "09", "10", "11", "12", "13", "14"),
  "Animal or vegetable oils & fats"=c("15"),
  "Prepared foodstuffs, beverages, spirits & tobacco"=c("16", "17", "18", "19", "20", "21", "22", "23", "24"),
  "Mineral products"=c("25", "26", "27"),
  "Chemicals and allied products"=c("28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38"),
  "Plastic, rubber & articles thereof"=c("39", "40"),
  "Raw hides, skins, leather articles & travel goods"=c("41", "42", "43"),
  "Wood, cork & articles thereof & plaiting material"=c("44", "45", "46"),
  "Wood pulp, paper & paperboard & articles thereof"=c("47", "48", "49"),
  "Textiles & textile articles"=c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63"),
  "Footwear, headgear, umbrellas & parts thereof"=c("64", "65", "66", "67"),
  "Articles of stone, plaster, cement, glass & ceremic products"=c("68", "69", "70"),
  "Pearls, precious & semi-precious stones & metals"=c("71"),
  "Base metals & articles thereof"=c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83"),
  "Machinery & mechanical & electrical appliances & parts thereof"=c("84", "85"),
  "Vehicles, aircraft & associated transport equipment"=c("86", "87", "88", "89"),
  "Photographic & optical, medical & surgical goods & clocks/watches & musical instruments"=c("90", "91", "92"),
  "Arms and ammunition, parts & accessories thereof"=c("93"),
  "Miscellaneous manufactured articles"=c("94", "95", "96"),
  "Works of art, collectors pieces & antiques"=c("97"),
  "Others"=c("98", "99")
)

# Build the table
importsByHS <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the  sub tables from the formatted excel table
importsByHSCodeSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="2_M", startRow=7, nColumns=25)

# Update the sub tables
importsByHSCodeSubTables <- updateSubTablesByTime(importsByHSCodeSubTables, month, year, importsByHS/1000000)

# Insert updated sub tables back into excel formatted table
if(is.null(importsByHSCodeSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="2_M", subTables=importsByHSCodeSubTables, nRowsInHeader=7,
                                         loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 2: Import by HS Code.\n")

#### Table 3: Export by HS Code ####

## Getting latest statistics ##
# Define the CP4 codes need for table (Exports)
codesCP4 <- c(1000)

# Define the categories used for each column
categoryColumn <- "HS.Code_2"
columnCategories <- list(
  "Live animals: animal products"=c("01", "02", "03", "04", "05"),
  "Vegetable products"=c("06", "07", "08", "09", "10", "11", "12", "13", "14"),
  "Animal or vegetable oils & fats"=c("15"),
  "Prepared foodstuffs, beverages, spirits & tobacco"=c("16", "17", "18", "19", "20", "21", "22", "23", "24"),
  "Mineral products"=c("25", "26", "27"),
  "Chemicals and allied products"=c("28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38"),
  "Plastic, rubber & articles thereof"=c("39", "40"),
  "Raw hides, skins, leather articles & travel goods"=c("41", "42", "43"),
  "Wood, cork & articles thereof & plaiting material"=c("44", "45", "46"),
  "Wood pulp, paper & paperboard & articles thereof"=c("47", "48", "49"),
  "Textiles & textile articles"=c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63"),
  "Footwear, headgear, umbrellas & parts thereof"=c("64", "65", "66", "67"),
  "Articles of stone, plaster, cement, glass & ceremic products"=c("68", "69", "70"),
  "Pearls, precious & semi-precious stones & metals"=c("71"),
  "Base metals & articles thereof"=c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83"),
  "Machinery & mechanical & electrical appliances & parts thereof"=c("84", "85"),
  "Vehicles, aircraft & associated transport equipment"=c("86", "87", "88", "89"),
  "Photographic & optical, medical & surgical goods & clocks/watches & musical instruments"=c("90", "91", "92"),
  "Arms and ammunition, parts & accessories thereof"=c("93"),
  "Miscellaneous manufactured articles"=c("94", "95", "96"),
  "Works of art, collectors pieces & antiques"=c("97"),
  "Others"=c("98", "99")
)

# Build the table
exportsByHS <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the  sub tables from the formatted excel table
exportsByHSCodeSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="3_DX", startRow=7, nColumns=25)

# Update the sub tables
exportsByHSCodeSubTables <- updateSubTablesByTime(exportsByHSCodeSubTables, month, year, exportsByHS/1000000)

# Insert updated sub tables back into excel formatted table
if(is.null(exportsByHSCodeSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="3_DX", subTables=exportsByHSCodeSubTables, nRowsInHeader=7,
                                         loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 3: Export by HS Code.\n")

#### Table 4: Re-Export by HS Code ####

## Getting latest statistics ##
# Define the CP4 codes need for table
codesCP4 <- c(3071)

# Define the categories used for each column
categoryColumn <- "HS.Code_2"
columnCategories <- list(
  "Live animals: animal products"=c("01", "02", "03", "04", "05"),
  "Vegetable products"=c("06", "07", "08", "09", "10", "11", "12", "13", "14"),
  "Animal or vegetable oils & fats"=c("15"),
  "Prepared foodstuffs, beverages, spirits & tobacco"=c("16", "17", "18", "19", "20", "21", "22", "23", "24"),
  "Mineral products"=c("25", "26", "27"),
  "Chemicals and allied products"=c("28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38"),
  "Plastic, rubber & articles thereof"=c("39", "40"),
  "Raw hides, skins, leather articles & travel goods"=c("41", "42", "43"),
  "Wood, cork & articles thereof & plaiting material"=c("44", "45", "46"),
  "Wood pulp, paper & paperboard & articles thereof"=c("47", "48", "49"),
  "Textiles & textile articles"=c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63"),
  "Footwear, headgear, umbrellas & parts thereof"=c("64", "65", "66", "67"),
  "Articles of stone, plaster, cement, glass & ceremic products"=c("68", "69", "70"),
  "Pearls, precious & semi-precious stones & metals"=c("71"),
  "Base metals & articles thereof"=c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83"),
  "Machinery & mechanical & electrical appliances & parts thereof"=c("84", "85"),
  "Vehicles, aircraft & associated transport equipment"=c("86", "87", "88", "89"),
  "Photographic & optical, medical & surgical goods & clocks/watches & musical instruments"=c("90", "91", "92"),
  "Arms and ammunition, parts & accessories thereof"=c("93"),
  "Miscellaneous manufactured articles"=c("94", "95", "96"),
  "Works of art, collectors pieces & antiques"=c("97"),
  "Others"=c("98", "99")
)

# Build the table
reExportsByHS <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the  sub tables from the formatted excel table
reExportsByHSCodeSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="4_ReX", startRow=7, nColumns=25)

# Update the sub tables
reExportsByHSCodeSubTables <- updateSubTablesByTime(reExportsByHSCodeSubTables, month, year, reExportsByHS/1000000)

# Insert updated sub tables back into excel formatted table
if(is.null(reExportsByHSCodeSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="4_ReX", subTables=reExportsByHSCodeSubTables, nRowsInHeader=7,
                                         loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 4: Re-Export by HS Code.\n")

#### Table 5: Total Exports by HS Code ####

## Getting latest statistics ##
# Calculate the total exports
totalExportsByHS <- exportsByHS + reExportsByHS

## Formatting the table ##
# Extract the  sub tables from the formatted excel table
totalExportsByHSCodeSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="5_TX", startRow=7, nColumns=25)

# Update the sub tables
totalExportsByHSCodeSubTables <- updateSubTablesByTime(totalExportsByHSCodeSubTables, month, year, totalExportsByHS/1000000)

# Insert updated sub tables back into excel formatted table
if(is.null(totalExportsByHSCodeSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="5_TX", subTables=totalExportsByHSCodeSubTables, nRowsInHeader=7,
                                         loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 5: Total Exports by HS Code.\n")

#### Table 6: Principle Exports ####

## Getting latest statistics ##
# Define the CP4 codes need for table
codesCP4 <- c(1000)

# Define the categories used for each column
categoryColumn <- "Principle.Exports"
columnCategories <- list(
  "Alcoholic Drinks"=c("Alcoholic Drinks"),
  "Beef fresh, chilled, frozen and preserved"=c("Beef fresh, chilled, frozen and preserved"),
  "Cocoa"=c("Cocoa"),
  "Coconut Meal"=c("Coconut Meal"),
  "Coconut oil"=c("Coconut Oil"),
  "Coffee, all types"=c("Coffee, all types"),
  "Copra"=c("Copra"),
  "Cowhides"=c("Cowhides"),
  "Fish: Tuna"=c("Fish: Tuna"),
  "Kava"=c("Kava"),
  "Ornamental fish"=c("Ornamental fish"),
  "Shell buttons"=c("Shell buttons"),
  "Vanilla"=c("Vanilla"),
  "Wood and articles of wood; wood charcoal"=c("Wood and articles of wood; wood charcoal"),
  "Other products"=c("Other products")
)

# Build the table
totalPrincipleExports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate the principle Re-Exports 

# Define the CP4 codes need for table
codesCP4 <- c(3071)

# Define the categories used for each column
categoryColumn <- "Principle.Exports"
columnCategories <- list(
  "Alcoholic Drinks"=c("Alcoholic Drinks"),
  "Tobacco and cigarettes"=c("Tobacco and cigarettes"),
  "Fuel"=c("Fuel"),
  "Other products"=c("Other products")
)

# Build the table
totalPrincipleReExports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Build a single vector storing the exports, re-exports and total
overallTotal <- sum(c(as.numeric(totalPrincipleExports[1, ]), as.numeric(totalPrincipleReExports[1, ])))
latestPrincipleExports <- c(NA, as.numeric(totalPrincipleExports[1, ]), NA, NA, as.numeric(totalPrincipleReExports[1, ]), NA, overallTotal)

# Extract the table from the formatted excel sheet
principleExportsTable <- read.xlsx(finalWorkbookFileName, sheet="6_PrinX", rows=5:31, skipEmptyRows=FALSE)

# Remove empty columns at end
principleExportsTable <- removeEmptyColumnsAtEnd(principleExportsTable)

# Add the latest month's statistics
principleExportsTable <- updateTableByCommodity(principleExportsTable, month, year, newStatistics=latestPrincipleExports / 1000000,
                                                numericColumns=2:ncol(principleExportsTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(principleExportsTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="6_PrinX", table=principleExportsTable, year=year,
                                                tableNumber="6", tableName="PRINCIPLE EXPORTS", boldRows=c(6,22,24,29,31), nRowsInNotes=4,
                                                numericColumns=2:ncol(principleExportsTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 6: Principle Exports.\n")

#### Table 7: Principle Imports ####

## Getting latest statistics ##
# Calculate the principle Imports 

# Define the CP4 codes need for table
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "Principle.Imports"
columnCategories <- list(
  "Meat and edible offal of poultry"=c("Meat and edible offal of poultry"),
  "Fish and crustaceans, molluscs and other aquatic invertebrates"=c("Fish and crustaceans, molluscs and other aquatic invertebrates"),
  "Milk and Cream"=c("Milk and Cream"),
  "Rice"=c("Rice"),
  "Wheat or meslin flour"=c("Wheat or meslin flour"),
  "Other prepared or preserved meat, meat offal or blood"=c("Other prepared or preserved meat, meat offal or blood"),
  "Prepared or preserved fish"=c("Prepared or preserved fish"),
  "Sugar"=c("Sugar"),
  "Pasta, whether cooked or stuffed or otherwise prepared, such as spaghetti, macaroni, noodles, lasagne, gnocchi, ravioli, cannelloni; couscous"=c("Pasta, whether cooked or stuffed or otherwise prepared, such as spaghetti, macaroni, noodles, lasagne, gnocchi, ravioli, cannelloni; couscous"),
  "Prepared foods obtained by the swelling or roasting of cereals or cereal products eg corn flakes; cereals (excl maize) in grain or flakes"=c("Prepared foods obtained by the swelling or roasting of cereals or cereal products eg corn flakes; cereals (excl maize) in grain or flakes"),
  "Bread, pastry, cakes, biscuits and other bakers' wares"=c("Bread, pastry, cakes, biscuits and other bakers' wares"),
  "Non-alcoholic drinks including mineral water"=c("Non-alcoholic drinks including mineral water"),
  "Alcoholic drinks"=c("Alcoholic drinks"),
  "Preparations of a kind used in animal feeding"=c("Preparations of a kind used in animal feeding"),
  "Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes"=c("Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes"),
  "Portland cement, aluminous cement, slag cement, supersulphate cement and similar hydraulic cements"=c("Portland cement, aluminous cement, slag cement, supersulphate cement and similar hydraulic cements"),
  "Petroleum oils and oils obtained from bituminous minerals"=c("Petroleum oils and oils obtained from bituminous minerals"),
  "Petroleum gases and other gaseous hydrocarbons"=c("Petroleum gases and other gaseous hydrocarbons"),
  "Medicaments"=c("Medicaments"),
  "Perfumes, make-up preparations and preparations for the care of the skin (excl medicaments) and products for hair"=c("Perfumes, make-up preparations and preparations for the care of the skin (excl medicaments) and products for hair"),
  "Tubes, pipes and hoses, and fittings therefor (for example, joints, elbows, flanges), of plastics"=c("Tubes, pipes and hoses, and fittings therefor (for example, joints, elbows, flanges), of plastics"),
  "Articles for the conveyance or packing of goods; stoppers, lids, caps and other closures, of plastics"=c("Articles for the conveyance or packing of goods; stoppers, lids, caps and other closures, of plastics"),
  "New pneumatic tires, of rubber"=c("New pneumatic tires, of rubber"),
  "Wood sawn or chipped lengthwise, sliced or peeled, whether or not planed, sanded or end-jointed"=c("Wood sawn or chipped lengthwise, sliced or peeled, whether or not planed, sanded or end-jointed"),
  "Toilet paper and similar paper"=c("Toilet paper and similar paper"),
  "Printed books, brochures, leaflets and similar printed matter"=c("Printed books, brochures, leaflets and similar printed matter"),
  "Articles of apparel and clothing accessories"=c("Articles of apparel and clothing accessories"),
  "Other made up textile articles; sets; worn clothing and worn textile articles; rags"=c("Other made up textile articles; sets; worn clothing and worn textile articles; rags"),
  "Footwear, gaiters and the like; parts of such articles"=c("Footwear, gaiters and the like; parts of such articles"),
  "Iron and steel"=c("Iron and steel"),
  "Articles of iron or steel"=c("Articles of iron or steel"),
  "Aluminium and articles thereof"=c("Aluminium and articles thereof"),
  "Spark-ignition reciprocating or rotary internal combustion piston engines and compression-ignition internal combustion piston engines (diesel or semi-diesel engines)"=c("Spark-ignition reciprocating or rotary internal combustion piston engines and compression-ignition internal combustion piston engines (diesel or semi-diesel engines)"),
  "Refrigerators, freezers and other refrigerating or freezing equipment, electric or other; heat pumps other than air conditioning machines"=c("Refrigerators, freezers and other refrigerating or freezing equipment, electric or other; heat pumps other than air conditioning machines"),
  "Centrifuges, including centrifugal dryers; filtering or purifying machinery and apparatus, for liquids or gases"=c("Centrifuges, including centrifugal dryers; filtering or purifying machinery and apparatus, for liquids or gases"),
  "Self-propelled bulldozers, angledozers, graders, levellers, scrapers, mechanical shovels, excavators, shovel loaders, tamping machines and road rollers"=c("Self-propelled bulldozers, angledozers, graders, levellers, scrapers, mechanical shovels, excavators, shovel loaders, tamping machines and road rollers"),
  "Automatic data processing machines; magnetic or optical readers, machines for transcribing data onto data media in coded form and machines for processing such data"=c("Automatic data processing machines; magnetic or optical readers, machines for transcribing data onto data media in coded form and machines for processing such data"),
  "Electric generating sets and rotary converters"=c("Electric generating sets and rotary converters"),
  "Telephone sets, including telephones for cellular networks or for other wireless networks"=c("Telephone sets, including telephones for cellular networks or for other wireless networks"),
  "Insulated wire, cable and other insulated electric conductors, whether or not fitted with connectors; optical fibre cables, made up of individually sheathed fibres, whether or not assembled with electric conductors or fitted with connectors"=c("Insulated wire, cable and other insulated electric conductors, whether or not fitted with connectors; optical fibre cables, made up of individually sheathed fibres, whether or not assembled with electric conductors or fitted with connectors"),
  "Motor vehicles for the transport of passengers"=c("Motor vehicles for the transport of passengers"),
  "Motor vehicles for the transport of goods"=c("Motor vehicles for the transport of goods"),
  "Parts of aircraft"=c("Parts of aircraft"),
  "Yachts and other vessels for pleasure or sports; rowing boats and canoes"=c("Yachts and other vessels for pleasure or sports; rowing boats and canoes"),
  "Other furniture and parts thereof"=c("Other furniture and parts thereof"),
  "Personal and household effects"=c("Personal and household effects"),
  "Other Imports"=c("Other Imports")
)

# Build the table
totalPrincipleImports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the table from the formatted excel sheet
principleImportsTable <- read.xlsx(finalWorkbookFileName, sheet="7_PrinM", rows=5:53, skipEmptyRows=FALSE)

# Remove empty columns at end
principleImportsTable <- removeEmptyColumnsAtEnd(principleImportsTable)

# Add the latest month's statistics
principleImportsTable <- updateTableByCommodity(principleImportsTable, month, year, newStatistics=totalPrincipleImports / 1000000, 
                                                numericColumns=2:ncol(principleImportsTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(principleExportsTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="7_PrinM", table=principleImportsTable, year=year,
                                                tableNumber="7", tableName="PRINCIPLE IMPORTS", boldRows=c(53), nRowsInNotes=2,
                                                numericColumns=2:ncol(principleImportsTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 7: Principle Imports.\n")

#### Table 8: Balance of Trade by Major Partner Countries ####

## Getting latest statistics ##
# Define the CP4 codes need for table
codesCP4 <- c(1000, 3071)

# Define the categories used for each column
categoryColumn <- "EXPORT.COUNTRY"
columnCategories <- list(
  "Australia: Exports"=c("AUSTRALIA"),
  "China, People's Republic of: Exports"=c("CHINA, PEOPLES REPUBLIC OF"),
  "Fiji: Exports"=c("FIJI"),
  "France: Exports"=c("FRANCE"),
  "Hong Kong: Exports"=c("HONG KONG"),
  "India: Exports"=c("INDIA"),
  "Indonesia: Exports"=c("INDONESIA"),
  "Japan: Exports"=c("JAPAN"),
  "Korea, Republic Of: Exports"=c("KOREA, REPUBLIC OF"),
  "Malaysia: Exports"=c("MALAYSIA"),
  "Netherlands: Exports"=c("NETHERLANDS"),
  "New Caledonia: Exports"=c("NEW CALEDONIA"),
  "New Zealand: Exports"=c("NEW ZEALAND"),
  "Papua New Guinea: Exports"=c("PAPUA NEW GUINEA"),
  "Philippines: Exports"=c("PHILIPPINES"),
  "Singapore: Exports"=c("SINGAPORE"),
  "Solomon Islands: Exports"=c("SOLOMON ISLANDS"),
  "Thailand: Exports"=c("THAILAND"),
  "United Kingdom: Exports"=c("UNITED KINGDOM"),
  "United States of America: Exports"=c("UNITED STATES OF AMERICA")
)
countries <- unique(processedTradeStats[, "EXPORT.COUNTRY"])
columnCategories[["All Others: Exports"]] <- countries[countries %in% unlist(columnCategories) == FALSE]

# Build the table
totalPartnerCountryExports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate the Imports by Major Partner Countries

# Define the CP4 codes need for table
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "IMPORT.COUNTRY"
columnCategories <- list(
  "Australia: Imports"=c("AUSTRALIA"),
  "China, People's Republic of: Imports"=c("CHINA, PEOPLES REPUBLIC OF"),
  "Fiji: Imports"=c("FIJI"),
  "France: Imports"=c("FRANCE"),
  "Hong Kong: Imports"=c("HONG KONG"),
  "India: Imports"=c("INDIA"),
  "Indonesia: Imports"=c("INDONESIA"),
  "Japan: Imports"=c("JAPAN"),
  "Korea, Republic Of: Imports"=c("KOREA, REPUBLIC OF"),
  "Malaysia: Imports"=c("MALAYSIA"),
  "Netherlands: Imports"=c("NETHERLANDS"),
  "New Caledonia: Imports"=c("NEW CALEDONIA"),
  "New Zealand: Imports"=c("NEW ZEALAND"),
  "Papua New Guinea: Imports"=c("PAPUA NEW GUINEA"),
  "Philippines: Imports"=c("PHILIPPINES"),
  "Singapore: Imports"=c("SINGAPORE"),
  "Solomon Islands: Imports"=c("SOLOMON ISLANDS"),
  "Thailand: Imports"=c("THAILAND"),
  "United Kingdom: Imports"=c("UNITED KINGDOM"),
  "United States of America: Imports"=c("UNITED STATES OF AMERICA")
)
countries <- unique(processedTradeStats[, "IMPORT.COUNTRY"])
columnCategories[["All Others: Imports"]] <- countries[countries %in% unlist(columnCategories) == FALSE]

# Build the table
totalPartnerCountryImports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate the Balance of Trade by Major Partner Countries

balanceOfTradeByMajorPartnerCountries <- totalPartnerCountryExports - totalPartnerCountryImports
colnames(balanceOfTradeByMajorPartnerCountries) <- sapply(colnames(balanceOfTradeByMajorPartnerCountries),
                                                          FUN=function(colName){
                                                            return(strsplit(colName, split=":")[[1]][1])
                                                          })

## Formatting the table ##
# Build a single vector aligned with the format of table 8: Exports, Imports, Balance for each major partner country
majorPartnerCountryStatistics <- c()
majorPartnerCountryStatistics[seq(from=1, by=3, length.out=length(totalPartnerCountryExports))] <- as.numeric(totalPartnerCountryExports)
majorPartnerCountryStatistics[seq(from=2, by=3, length.out=length(totalPartnerCountryImports))] <- as.numeric(totalPartnerCountryImports)
majorPartnerCountryStatistics[seq(from=3, by=3, length.out=length(balanceOfTradeByMajorPartnerCountries))] <- as.numeric(balanceOfTradeByMajorPartnerCountries)

# Extract the table from the formatted excel sheet
balanceOfTradeMajorTable <- read.xlsx(finalWorkbookFileName, sheet="8_BOT_PC", rows=5:71, skipEmptyRows=FALSE)

# Remove empty columns at end
balanceOfTradeMajorTable <- removeEmptyColumnsAtEnd(balanceOfTradeMajorTable)

# Add the latest month's statistics
balanceOfTradeMajorTable <- updateTableByCommodity(balanceOfTradeMajorTable, month, year, 
                                                   newStatistics=majorPartnerCountryStatistics / 1000000, 
                                                   numericColumns=3:ncol(balanceOfTradeMajorTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(balanceOfTradeMajorTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="8_BOT_PC", table=balanceOfTradeMajorTable, year=year, 
                                                tableNumber="8", tableName="BALANCE OF TRADE BY MAJOR PARTNER COUNTRIES", 
                                                boldRows=c(69, 70, 71), nRowsInNotes=2,
                                                numericColumns=3:ncol(balanceOfTradeMajorTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 8: Balance of Trade by Major Partner Countries.\n")

#### Table 9: Balance of Trade by Region ####

## Getting latest statistics ##
# Define the CP4 codes need for table
codesCP4 <- c(1000, 3071)

# Define the categories used for each column
categoryColumn <- "EXPORT.REGION"
columnCategories <- list(
  "Africa: Exports"=c("AFRICA"),
  "The Americas: Exports"=c("AMERICAS"),
  "Asia: Exports"=c("ASIA"),
  "Europe: Exports"=c("EUROPE"),
  "Oceania: Exports"=c("OCEANIA"),
  "Various: Exports"=c("VARIOUS")
)

# Build the table
totalPartnerRegionExports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)


# Calculate the Imports by Region

# Define the CP4 codes need for table
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "IMPORT.REGION"
columnCategories <- list(
  "Africa: Imports"=c("AFRICA"),
  "The Americas: Imports"=c("AMERICAS"),
  "Asia: Imports"=c("ASIA"),
  "Europe: Imports"=c("EUROPE"),
  "Oceania: Imports"=c("OCEANIA"),
  "Various: Imports"=c("VARIOUS")
)

# Build the table
totalPartnerRegionImports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate the Balance of Trade by Region

balanceOfTradeByRegion <- totalPartnerRegionExports - totalPartnerRegionImports
colnames(balanceOfTradeByRegion) <- sapply(colnames(balanceOfTradeByRegion),
                                           FUN=function(colName){
                                             return(strsplit(colName, split=":")[[1]][1])
                                           })

# Calculate the Balance of Trade for the Pacific Islands
pictsExports <- totalPartnerRegionExports["Oceania: Exports"] - totalPartnerCountryExports["New Zealand: Exports"] - totalPartnerCountryExports["Australia: Exports"]
pictsImports<- totalPartnerRegionImports["Oceania: Imports"] - totalPartnerCountryImports["New Zealand: Imports"] - totalPartnerCountryImports["Australia: Imports"]
pictsBalance<- balanceOfTradeByRegion["Oceania"] - balanceOfTradeByMajorPartnerCountries["Australia"] - balanceOfTradeByMajorPartnerCountries["New Zealand"]

## Formatting the table ##
# Build a single vector aligned with the format of table 8: Exports, Imports, Balance for each region
majorRegionStatistics <- c()
majorRegionStatistics[seq(from=1, by=3, length.out=length(totalPartnerRegionExports))] <- as.numeric(totalPartnerRegionExports)
majorRegionStatistics[seq(from=2, by=3, length.out=length(totalPartnerRegionImports))] <- as.numeric(totalPartnerRegionImports)
majorRegionStatistics[seq(from=3, by=3, length.out=length(balanceOfTradeByRegion))] <- as.numeric(balanceOfTradeByRegion)

# Add in pacific islands statistics
majorRegionStatistics <- c(majorRegionStatistics, NA, as.numeric(pictsExports), as.numeric(pictsImports), as.numeric(pictsBalance))

# Extract the table from the formatted excel sheet
balanceOfTradeRegionsTable <- read.xlsx(finalWorkbookFileName, sheet="9_Trade_Reg", rows=5:30, skipEmptyRows=FALSE)

# Remove empty columns at end
balanceOfTradeRegionsTable <- removeEmptyColumnsAtEnd(balanceOfTradeRegionsTable)

# Add the latest month's statistics
balanceOfTradeRegionsTable <- updateTableByCommodity(balanceOfTradeRegionsTable, month, year, 
                                                     newStatistics=majorRegionStatistics / 1000000, 
                                                     numericColumns=3:ncol(balanceOfTradeRegionsTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(balanceOfTradeRegionsTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="9_Trade_Reg", table=balanceOfTradeRegionsTable, year=year,
                                                tableNumber="9", tableName="TRADE BY REGION", 
                                                boldRows=c(24, 25, 26), nRowsInNotes=2,
                                                numericColumns=3:ncol(balanceOfTradeRegionsTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}


# Print progress
cat("Finished formatting Table 9: Balance of Trade by Region.\n")

#### Table 10: Trade by Mode of Transport ####

## Getting latest statistics ##

# Calculate Export Trade by Mode of Transport
# Define the CP4 codes need for table
codesCP4 <- c(1000, 3071)

# Define the categories used for each column
categoryColumn <- "Mode.of.Transport"
columnCategories <- list(
  "Air: Exports"=c("AIR"),
  "Water: Exports"=c("SEA"),
  "Land: Exports"=c("LAND"),
  "Postal: Exports"=c("POSTAGE"),
  "Other: Exports"=c("OTHER")
)

# Build the table
totalTransportExports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate Import Trade by Mode of Transport
# Define the CP4 codes need for table
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "Mode.of.Transport"
columnCategories <- list(
  "Air: Imports"=c("AIR"),
  "Water: Imports"=c("SEA"),
  "Land: Imports"=c("LAND"),
  "Postal: Imports"=c("POSTAGE"),
  "Other: Imports"=c("OTHER")
)

# Build the table
totalTransportImports <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

# Calculate Trade by Mode of Transport
tradebyModeOfTransport<- totalTransportExports + totalTransportImports

## Formatting the table ##
tradeByModeOfTransport <- data.frame(rbind(as.numeric(totalTransportExports), as.numeric(totalTransportImports)))
colnames(tradeByModeOfTransport) <- c(unlist(columnCategories), "Total")

# Extract the  sub tables from the formatted excel table
tradeByModeOfTransportSubTables <- extractSubTablesFromFormattedTableByTime(finalWorkbookFileName, sheet="10_Trade_Trspt", startRow=8, nColumns=8, numericColumns=3:8)

# Update the sub tables
tradeByModeOfTransportSubTables <- updateSubTablesByTime(tradeByModeOfTransportSubTables, month, year, tradeByModeOfTransport/1000000, monthColumn="Year")

# Insert updated sub tables back into excel formatted table
if(is.null(tradeByModeOfTransportSubTables) == FALSE){
  finalWorkbook <- insertUpdatedSubTablesAsFormattedTable(finalWorkbookFileName, sheet="10_Trade_Trspt", subTables=tradeByModeOfTransportSubTables, 
                                         nRowsInHeader=8, loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 10: Trade by Mode of Transport.\n")

#### Table 11: Trade by Trade Agreement (INCOMPLETE) ####

## Getting latest statistics ##

# Get the trade stats for the 
tradeStatsForMSG <- processedTradeStats[is.na(processedTradeStats$PRF) == FALSE & processedTradeStats$PRF == "MSG", ]

# Aggregate the statistical value column by country and SITC (note import and export countries always appear to be equal)
msgLatestStats <- aggregate(tradeStatsForMSG$Stat..Value, by=list(tradeStatsForMSG$SITC_1, 
                                                                  tradeStatsForMSG$IMPORT.COUNTRY),
                            FUN=sum, na.rm=TRUE)
colnames(msgLatestStats) <- c("SITC_1", "COUNTRY", "Stat..Value")

# Create a table to store the data in
templateMSGTable <- data.frame(matrix(nrow=10, ncol=length(unique(msgLatestStats$COUNTRY))))
rownames(templateMSGTable) <- 0:9
colnames(templateMSGTable) <- sort(unique(msgLatestStats$COUNTRY))

# Fill in the values from this month
for(row in seq_len(nrow(msgLatestStats))){
  templateMSGTable[msgLatestStats[row, "SITC_1"], msgLatestStats[row, "COUNTRY"]] <- msgLatestStats[row, "Stat..Value"]
}

# Add total column
templateMSGTable$Total <- rowSums(templateMSGTable, na.rm=TRUE)
templateMSGTable[templateMSGTable == 0] <- NA

## Formatting the table ##

# Write the MSG trade stats values from current month into table (note that no historic data in Table 11 - so all values are overwritten)
openxlsx::writeData(finalWorkbook, sheet="11_TradeAg", startCol=2, startRow=3, x=templateMSGTable, colNames=FALSE)

# Print progress
cat("Finished updating values in Table 11: Trade by Trade Agreement.\n")

#### Table 12: Exports by SITC  ####

## Getting latest statistics ##
# Calculate Exports by SITC 

# Define the CP4 codes need for table
codesCP4 <- c(1000, 3071)

# Define the categories used for each column
categoryColumn <- "SITC_1"
columnCategories <- list(
  "Food and live animals"=c("0"),
  "Beverages and tobacco"=c("1"),
  "Crude materials, inedible, except fuels"=c("2"),
  "Mineral fuels, lubricants and related materials"=c("3"),
  "Animal and vegetable oils, fats and waxes"=c("4"),
  "Chemicals and related products, not elsewhere specified"=c("5"),
  "Manufactured goods classified chiefly by material"=c("6"),
  "Machinery and transport equipment"=c("7"),
  "Miscellaneous manufactured goods"=c("8"),
  "Commodities and transactions not classified elsewhere in SITC"=c("9")
)

# Build the table
totalExportsbySITC <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the table from the formatted excel sheet
totalExportsbySITCTable <- read.xlsx(finalWorkbookFileName, sheet="12_X_SITC", rows=5:16, skipEmptyRows=FALSE)

# Remove empty columns at end
totalExportsbySITCTable <- removeEmptyColumnsAtEnd(totalExportsbySITCTable)

# Add the latest month's statistics
totalExportsbySITCTable <- updateTableByCommodity(totalExportsbySITCTable, month, year, 
                                                  newStatistics=totalExportsbySITC / 1000000, 
                                                  numericColumns=3:ncol(totalExportsbySITCTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(totalExportsbySITCTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="12_X_SITC", table=totalExportsbySITCTable, year=year,
                                                tableNumber="12", tableName="EXPORTS BY SITC", boldRows=c(16), nRowsInNotes=3,
                                                numericColumns=3:ncol(totalExportsbySITCTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 12: Exports by SITC.\n")

#### Table 13: Imports by SITC ####

## Getting latest statistics ##
# Calculate Exports by SITC 

# Define the CP4 codes need for table
codesCP4 <- c(4000, 4071, 7100)

# Define the categories used for each column
categoryColumn <- "SITC_1"
columnCategories <- list(
  "Food and live animals"=c("0"),
  "Beverages and tobacco"=c("1"),
  "Crude materials, inedible, except fuels"=c("2"),
  "Mineral fuels, lubricants and related materials"=c("3"),
  "Animal and vegetable oils, fats and waxes"=c("4"),
  "Chemicals and related products, not elsewhere specified"=c("5"),
  "Manufactured goods classified chiefly by material"=c("6"),
  "Machinery and transport equipment"=c("7"),
  "Miscellaneous manufactured goods"=c("8"),
  "Commodities and transactions not classified elsewhere in SITC"=c("9")
)

# Build the table
totalImportsbySITC <- buildRawSummaryTable(processedTradeStats, codesCP4, categoryColumn, columnCategories)

## Formatting the table ##
# Extract the table from the formatted excel sheet
totalImportsbySITCTable <- read.xlsx(finalWorkbookFileName, sheet="13_M_SITC", rows=5:16, skipEmptyRows=FALSE)

# Remove empty columns at end
totalImportsbySITCTable <- removeEmptyColumnsAtEnd(totalImportsbySITCTable)

# Add the latest month's statistics
totalImportsbySITCTable <- updateTableByCommodity(totalImportsbySITCTable, month, year, 
                                                  newStatistics=totalImportsbySITC / 1000000, 
                                                  numericColumns=3:ncol(totalImportsbySITCTable))

# Insert the updated table back into the formatted excel sheet
if(is.null(totalImportsbySITCTable) == FALSE){
  finalWorkbook <- insertUpdatedTableByCommodityAsFormattedTable(finalWorkbookFileName, sheet="13_M_SITC", table=totalImportsbySITCTable, year=year,
                                                tableNumber="13", tableName="IMPORTS BY SITC", boldRows=c(16), nRowsInNotes=2,
                                                numericColumns=3:ncol(totalImportsbySITCTable),
                                                loadAndSave=FALSE, finalWorkbook=finalWorkbook)
}

# Print progress
cat("Finished formatting Table 13: Imports by SITC.\n")

#### Finish ####

# Save the changes to the excel file
openxlsx::saveWorkbook(finalWorkbook, file=finalWorkbookFileName, overwrite=TRUE)

# Print progress
cat(paste0("Finished updated formatted excel tables in ", finalWorkbookFileName, ".\n"))
