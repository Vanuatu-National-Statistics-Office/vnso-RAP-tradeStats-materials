#' Use \code{kableExtra} package to create nice scrollable table in Rmarkdown
#'
#' A function to change the alpha value (transparency) of colours that are defined as strings.
#' @param table An R object, typically a matrix or data frame.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have 100% width. Defaults to \code{FALSE}.
#' @param position A character string determining how to position the table on a page. Possible values include \code{left}, \code{center}, \code{right}, \code{float_left} and \code{float_right}. 
#' @param bootstrap_options A character vector for bootstrap table options. Possible options include \code{basic}, \code{striped}, \code{bordered}, \code{hover}, \code{condensed} and \code{responsive}.
#' @param ... Arguments to be passed to \code{kableExtra::kable_styling} function
#' @keywords kable markdown kableExtra
#' @examples
#' # Define a data frame
#' data <- data.frame("X"=rnorm(100), "Y"=1:100)
#' 
#' # Create the interactive table
#' prettyTable(data)
prettyTable <- function(table, full_width=FALSE, position="left", bootstrap_options="striped", ...){
  
  # Use the kable function to create a nicer formatted table
  knitr::kable(table) %>%
    
    # Set the format
    kableExtra::kable_styling(bootstrap_options=bootstrap_options, # Set the colour of rows
                              full_width=full_width, # Make the table not stretch to fit the page
                              position=position, 
                              ...) %>% # Position the table on the left
    
    # Make the table scrollable
    scroll_box(height = "400px")
}

#' Extracting the Balance of Trade historic sub-tables from excel workbook with final formatted tbales
#' 
#' A function to extract the pre-inserted historic data in a currently formatted Balance of Trade Table
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @return Returns list with dataframes representing the Annual and Monthly sub tables and notes
#' @keywords BoT openxlsx
extractHistoricBalanceOfTradeStatistics <- function(fileName){
  
  # Extract the full historic table
  historic <- openxlsx::read.xlsx(fileName, sheet="1_BOT", skipEmptyRows=FALSE, startRow=5)
  
  # Identify when each sub-table starts
  annualStart <- 1
  monthlyStart <- which(historic[, 1] == "Monthly")
  end <- which(historic[, 1] == "Notes:") - 1
  
  # Extract the Annual statistics
  annually <- historic[1:(monthlyStart-1), ]
  colnames(annually) <- c("Year", "blank", "Export", "Re-Export", "Total Export", "Imports CIF", "Trade Balance")
  annually$`Total Export` <- as.numeric(annually$`Total Export`)
  annually$Year <- as.numeric(annually$Year)
  
  # Extract the monthly statistics
  monthly <- historic[(monthlyStart+1):end, ]
  colnames(monthly) <- c("Year", "Month", "Export", "Re-Export", "Total Export", "Imports CIF", "Trade Balance")
  monthly$`Total Export` <- as.numeric(monthly$`Total Export`)
  monthly$Year <- as.numeric(monthly$Year)
  
  # Extract the notes from the table
  notes <- historic[(end+1):nrow(historic), c(1,2)]
  colnames(notes) <- c("Notes", "blank")
  
  # Return the extract sub tables
  return(list("Annually"=annually, "Monthly"=monthly, "Notes"=notes))
}

#' Update Annual and Monthly sub-tables of the Balance of Trade table
#' 
#' A function that informatively updates the annual and monthly tables. In December add row to annual table. In January include year in monthly table.
#' @param subTables A list with dataframes representing the Annual and Monthly sub tables
#' @param month Full name of month with first letter upper case
#' @param year Full year
#' @param newTradeBalanceStatistics Data.frame with single row storing balance of trade statistics for current month
#' @return Returns list with updated dataframes representing the Annual and Monthly sub tables and notes
#' @keywords BoT openxlsx
updateBalanceOfTradeSubTables <- function(subTables, month, year, newTradeBalanceStatistics){
  
  # Check if December
  december <- month == "December"
  
  # Check if we need to update the annual table
  if(month == "December"){
    
    # Calculate the sum of statistics for the last year
    januaryRows <- which(subTables$Monthly == "January")
    lastYearsRows <- januaryRows[length(januaryRows)]:nrow(subTables$Monthly)
    currentYearsTotals <- data.frame("Year"=year, "blank"=NA,
                                     "Export"=sum(subTables$Monthly$Export[lastYearsRows], na.rm=TRUE) + newTradeBalanceStatistics$Export,
                                     "Re-Export"=sum(subTables$Monthly$`Re-Export`[lastYearsRows], na.rm=TRUE) + newTradeBalanceStatistics$`Re-Export`,
                                     "Total Export"=sum(subTables$Monthly$`Total Export`[lastYearsRows], na.rm=TRUE) + newTradeBalanceStatistics$`Total Export`,
                                     "Imports CIF"=sum(subTables$Monthly$`Imports CIF`[lastYearsRows], na.rm=TRUE) + newTradeBalanceStatistics$`Imports CIF`,
                                     "Trade Balance"=sum(subTables$Monthly$`Trade Balance`[lastYearsRows], na.rm=TRUE) + newTradeBalanceStatistics$`Trade Balance`,
                                     stringsAsFactors=FALSE)
    
    # Add a new row to the annual table for this years data
    subTables$Annually <- rbind(subTables$Annually, currentYearsTotals)
  }
  
  # Update the monthly table
  newTradeBalanceStatistics$Year <- ifelse(month == "January", as.numeric(year), NA)
  newTradeBalanceStatistics$Month <- month
  if(month == "December"){
    newTradeBalanceStatistics <- rbind(newTradeBalanceStatistics, NA)
  }
  subTables$Monthly <- rbind(subTables$Monthly, newTradeBalanceStatistics)
  
  return(subTables)
}

#' Inserts updated Balance of Trade sub tables back into formatted excel sheet
#' 
#' A function that inserts the updated Annual and Monthly statistics back into formatted Balance of Trade table 
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @param subTables A list with dataframes representing the Annual and Monthly sub tables
#' @param nRowsInHeader The number of rows that make up the header of the formatted Balance of Trade table
#' @keywords BoT openxlsx
updatedFinalFormattedBalanceOfTradeTable <- function(fileName, subTables, nRowsInHeader=6){

  # Calculate the number of rows taken up by each sub table
  nRowsInAnnual <- nrow(subTables$Annually)
  nRowsInMonthly <- nrow(subTables$Monthly)
  nRows <- nRowsInAnnual + 2 + nRowsInMonthly
  
  # Load the final tables workbook for editing
  finalWorkbook <- openxlsx::loadWorkbook(fileName)

  # Clear the current contents of the workbook
  openxlsx::writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader, x=matrix(NA, nrow=nRows+4, ncol=7), colNames=FALSE)
  openxlsx::removeCellMerge(finalWorkbook, sheet="1_BOT", cols=1:7, rows=nRowsInHeader:(nRowsInHeader+nRows))
  openxlsx::setRowHeights(finalWorkbook, sheet="1_BOT", rows=nRowsInHeader:(nRowsInHeader+nRows), heights=14.5)
  
  # Insert the Annual sub table
  openxlsx::writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader, x=subTables$Annually, colNames=FALSE)
  
  # Insert the monthly sub table
  writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRowsInAnnual, x="Monthly", colNames=FALSE)
  openxlsx::writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRowsInAnnual+1, x=subTables$Monthly, colNames=FALSE)
  
  # Insert the notes
  openxlsx::writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRows, x=subTables$Notes, colNames=FALSE)
 
  # Apply a general formatting to the table contents
  default <- openxlsx::createStyle(borderStyle="thin", borderColour="black", border=c("top", "bottom", "left", "right"), fontName="Times New Roman")
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=default, gridExpand=TRUE, cols=1:7, rows=nRowsInHeader:(nRowsInHeader+nRows+4), stack=FALSE)
  
  # Format the Monthly sub table name as bold
  bold <- openxlsx::createStyle(textDecoration="bold")
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=bold, rows=nRowsInHeader+nRowsInAnnual, cols=1, stack=TRUE)
  
  # Format the balance of trade statistics as numbers
  number <- openxlsx::createStyle(numFmt="#,##0")
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=number, gridExpand=TRUE, stack=TRUE, cols=3:7,
                     rows=nRowsInHeader:(nRowsInHeader+nRows))
  
  # Format the year values in table as numbers
  number <- openxlsx::createStyle(numFmt="0")
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=number, stack=TRUE, cols=1, gridExpand=TRUE,
                     rows=nRowsInHeader:(nRowsInHeader+nRowsInAnnual-2))
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=number, stack=TRUE, cols=1, gridExpand=TRUE,
                     rows=(nRowsInHeader+nRowsInAnnual+1):(nRowsInHeader+nRows-2))

  # Format the notes as italics
  italics <- openxlsx::createStyle(textDecoration="italic")
  openxlsx::addStyle(finalWorkbook, sheet="1_BOT", style=italics, cols=1:2, stack=TRUE, gridExpand=TRUE,
                     rows=(nRowsInHeader+nRows):(nRowsInHeader+nRows+4))
  
  # Save the edited workbook as a new file
  editedFinalWorkbookFileName <- gsub(pattern=".xlsx", replacement="_EDITED.xlsx", fileName)
  openxlsx::saveWorkbook(finalWorkbook, file=editedFinalWorkbookFileName, overwrite=TRUE)
}


#' Extract the subset codes from the original 8 digit HS codes or 6 digit SITC codes
#' 
#' A function that given a length of subset to extract, it will subset each code in a vector from the large original codes. Designed to work with 8 digit HS codes or 6 digit SITC codes.
#' @param codes A vector of character vectors (strings) representing an 8 digit HS code or 6 digit SITC code
#' @param nDigits A integer defining the length of the output HS code. Expecting values 6, 4, or 2nD
#' @return Returns a character vector representing a subset of the digits within the 8 digit HS code provided. Length defined by \code{digits} parameter.
#' @keywords substr HSCode
#' @examples
#' # Define a vector of HS code
#' hsCodes <- c("08099912", "08099912", "08099912", "08099912")
#' 
#' # Apply the function to the vector of HS codes
#' hsCodes_6 <- extractCodeSubset(hsCodes, nDigits=6)
extractCodeSubset <- function(codes, nDigits){

  # Check if the number of digits is negative
  if(nDigits <= 0){
    stop("The number of digits to extract must be more than 0")
  }
  
  # Extracting the subset of the 8 digit HS code
  subsettedCodes <- sapply(codes, 
                           FUN=function(code, nDigits){
                             
                             # Check if the number of digits we want to select is less than or equal 8
                             if(nDigits > nchar(code)){
                               warning(paste0("The number of digits (", nDigits, ") to extract was more than the length of the code (", code, ") provided"))
                               return(NA)
                             }
                             
                             # Extract the subset of the current code
                             subset <- substr(code, start=1, stop=nDigits)
                             
                             return(subset)
                           }, nDigits)
  
  return(as.vector(subsettedCodes))
}

#' Build summary table that sums statistical value of exports or imports according to their category 
#' 
#' A function that will match according to export or import and given a category to extraction, will calculate its statistical value for each column in summary table. 
#' @param processedTradeStats A dataframe containing the cleaned Customs data, including all classifications 
#' @param codesCP4 Customs extended procedure codes that are representative of exports (1000), re-exports (3071) and imports (4000, 4071, 7100)
#' @param categoryColumn A column header used to define the subset of data that needs to be summed  
#' @param columnCategories A list providing a vector of categories to be used to select data in \code{categoryColumn} for each column 
#' @return Returns an integer vector representing a subset of the dataframe processedTradeStats
buildRawSummaryTable <- function(processTradeStats, codesCP4, categoryColumn, columnCategories){
  
  # Initialise a dataframe to store the calculated statistics
  summaryTable <- data.frame(matrix(NA, nrow=1, ncol=length(columnCategories)), check.names=FALSE, stringsAsFactors=FALSE)
  colnames(summaryTable) <- names(columnCategories)
  
  # Calculate the sum of products matching CP4 code and category for each column
  for(column in names(columnCategories)){
    
    # Calculate sum of products matching CP4 code and category for the current column
    summaryTable[1, column] <- sum(processedTradeStats[processedTradeStats$CP4 %in% codesCP4 &
                                                          processedTradeStats[, categoryColumn] %in% columnCategories[[column]], 
                                                       "Stat..Value"],
                                   na.rm= TRUE)
  }
  
  # Calculate the total
  summaryTable$Total <- sum(summaryTable[, ], na.rm=TRUE)
  
  return(summaryTable)
}

#' Calculate the statistical value of exports and imports according to their category 
#' 
#' A function that will match according to export or import and given a category to extraction, will calculate its statistical value. 
#' @param processedTradeStats A dataframe containing the cleaned Customs data, including all classifications 
#' @param codes_CP4 Customs extended procedure codes that are representative of exports (1000), re-exports (3071) and imports (4000, 4071, 7100)
#' @param categoryCol A column header used to define the subset of data that needs to be summed  
#' @param categoryValues A variable use to define the statistical value 
#' @return Returns an integer vector representing a subset of the dataframe processedTradeStats
calculateStatValueSum<- function(processedTradeStats, codes_CP4, categoryCol, categoryValues){
  
  # Calculate sum of products matching CP4 code and category 
  statValueSum<- sum(processedTradeStats[processedTradeStats$CP4 %in% codes_CP4 &
                                           processedTradeStats[, categoryCol] %in% categoryValues, 
                                         "Stat..Value"], na.rm= TRUE)
  # Return the sum
  return(statValueSum)
}

#' Check if classification not present after merging
#' 
#' A function that searches column added after merging to identify if any values missing
#' @param merged A data.frame resulting from a merge operation
#' @param by The column used as common identifier in merge operation
#' @param column A column that was pulled in during merge
searchForMissingObservations <- function(merged, by, column){
  
  # Check if any NA values are present in column of interest
  naIndices <- which(is.na(merged[, column]))
  
  # If NAs are present report the category they are present for
  for(index in naIndices){
    
    warning(paste0("No observation present in classification table for \"", merged[index, by]), "\" in ", by, "\n")
  }
}
