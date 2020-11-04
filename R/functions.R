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

#' Extracting the reported statistics in sub-tables from sheet in excel workbook with final formatted tables
#' 
#' A function to extract the pre-inserted historic data in a currently formatted sheet
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @param sheet A character string identifying the sheet to extract data from
#' @param startRow An integer specifying the row that the Annual table starts in
#' @return Returns list with dataframes representing the Annual and Monthly sub tables and notes
#' @keywords openxlsx
extractSubTablesFromFormattedTableByTime <- function(fileName, sheet, startRow, nColumns=NULL){
  
  # Extract the full historic table
  historic <- openxlsx::read.xlsx(fileName, sheet=sheet, skipEmptyRows=FALSE, startRow=startRow)
  if(is.null(nColumns) == FALSE){
    historic <- historic[, seq_len(nColumns)]
  }
  
  # Identify when each sub-table starts
  annualStart <- 1
  monthlyStart <- which(historic[, 1] == "Monthly")
  end <- which(historic[, 1] == "Notes:") - 1
  
  # Extract the Annual statistics
  annually <- historic[1:(monthlyStart-1), ]
  colnames(annually)[c(1,2)] <- c("Year", "blank")
  annually <- annually[is.na(annually$Year) == FALSE, ]
  for(columnIndex in c(1, 3:ncol(annually))){
    annually[, columnIndex] <- as.numeric(annually[, columnIndex])
  }

  # Extract the monthly statistics
  monthly <- historic[(monthlyStart+1):end, ]
  colnames(monthly)[c(1,2)] <- c("Year", "Month")
  for(columnIndex in seq_len(ncol(monthly))[-2]){
    monthly[, columnIndex] <- as.numeric(monthly[, columnIndex])
  }

  # Extract the notes from the table
  notes <- historic[(end+1):nrow(historic), c(1,2)]
  colnames(notes) <- c("Notes", "blank")
  
  # Return the extract sub tables
  return(list("Annually"=annually, "Monthly"=monthly, "Notes"=notes))
}

#' Update Annual and Monthly sub-tables of specific formatted table (commodities as columns, time as rows)
#' 
#' A function that informatively updates the annual and monthly tables. In December add row to annual table.
#' @param subTables A list with dataframes representing the Annual and Monthly sub tables
#' @param month Full name of month with first letter upper case
#' @param year Full year
#' @param newStatistics Data.frame with single row storing statistics for current month to be added to table
#' @return Returns list with updated dataframes representing the Annual and Monthly sub tables and notes
#' @keywords openxlsx
updateSubTablesByTime <- function(subTables, month, year, newStatistics){

  # Check if data have already been inserted into sub tables
  latestYearInSubTables <- max(subTables$Monthly$Year, na.rm=TRUE)
  months <- subTables$Monthly$Month[is.na(subTables$Monthly$Month) == FALSE]
  latestMonth <- months[length(months)]
  if(latestYearInSubTables == year && latestMonth == month){
    warning("Sub tables already contain data for specified month.")
    return(NULL)
  }
    
  # Note the number of columns in the sub tables
  nColumns <- ncol(subTables$Monthly)

  # Check if we need to update the annual table
  if(month == "January"){
    
    # Calculate the sum of statistics for the last year
    januaryRows <- which(subTables$Monthly$Month == "January")
    lastYearsRows <- januaryRows[length(januaryRows)]:nrow(subTables$Monthly)
    currentYearsTotals <- data.frame("Year"=as.numeric(year), "blank"=NA, stringsAsFactors=FALSE)
    currentYearsTotals[, colnames(subTables$Annually)[3:nColumns]] <- 
      colSums(subTables$Monthly[lastYearsRows, 3:nColumns], na.rm=TRUE) + newStatistics
    
    # Add a new row to the annual table for this years data
    subTables$Annually <- rbind(subTables$Annually, currentYearsTotals)
  }
  
  # Update the monthly table
  names(newStatistics) <- colnames(subTables$Monthly)[3:nColumns] 
  newStatistics$Year <- ifelse(month == "January", as.numeric(year), NA)
  newStatistics$Month <- month
  if(month == "December"){
    newStatistics <- rbind(newStatistics, NA)
  }
  subTables$Monthly <- rbind(subTables$Monthly, newStatistics)
  
  return(subTables)
}

#' Update a structured table that has statistics oriented with time (years and then months) in columns and commodities as rows
#' 
#' A function that informatively updates the annual and monthly stored in a structured table (time as columns, commodities as rows)
#' @param subTables A list with dataframes representing the Annual and Monthly sub tables
#' @param month Full name of month with first letter upper case
#' @param year Full year
#' @param newStatistics A vector representing the statistics for latest month (ordered by rows in structured table)
#' @return Returns structured table (data.frame) with latest month's data inserted
#' @keywords openxlsx
updateTableByCommodity <- function(structuredTable, month, year, newStatistics, numericColumns=NULL){
  
  # Check if data have already been inserted into sub tables
  colNames <- colnames(structuredTable)
  if(year %in% colNames && grepl(colNames[length(colNames)], pattern=substr(month, 1, 3))){
    warning("Table already contains data for specified month.")
    return(NULL)
  }
  
  # Check the class of the new statistics to add
  if(class(newStatistics) == "data.frame"){
    newStatistics <- as.numeric(newStatistics[1, ])
  }
  
  # Convert the numeric columns to numeric if not already
  if(is.null(numericColumns) == FALSE){
    for(column in numericColumns){
      structuredTable[, column] <- as.numeric(structuredTable[, column])
    }
  }
  
  # Add the latest month's statistics
  nColumns <- ncol(structuredTable)
  structuredTable[, nColumns + 1] <- newStatistics
  colnames(structuredTable)[nColumns + 1] <- substr(month, 1, 3)
  nColumns <- nColumns + 1
  
  # Check if January - then the annual section of table needs to be updated as well
  if(month == "January"){
    
    # Calculate the sum for each commodity over the previous year
    naCounts <- unlist(apply(structuredTable[, (nColumns - 12):(nColumns - 1)], MARGIN=1,
                             FUN=function(values){
                                return(sum(is.na(values)))
                             }))
    previousYearsTotals <- rowSums(structuredTable[, (nColumns - 12):(nColumns - 1)], na.rm=TRUE)
    previousYearsTotals[naCounts == 12] <- NA # Not calculating row sum for empty rows in table
    
    # Identify the column where the annual table ends
    lastAnnualColumn <- which(grepl(colnames(structuredTable), pattern="Jan"))[1] - 1
    
    # Insert the data for current year
    structuredTable <- cbind(structuredTable[, 1:lastAnnualColumn], previousYearsTotals, structuredTable[, (lastAnnualColumn+1):nColumns])
    colnames(structuredTable)[lastAnnualColumn+1] <- year
  }
  
  return(structuredTable)
}

#' Inserts updated sub tables back into formatted excel sheet
#' 
#' A function that inserts the updated Annual and Monthly statistics back into formatted Balance of Trade table 
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @param sheet A character string identifying the sheet to extract data from
#' @param subTables A list with dataframes representing the Annual and Monthly sub tables
#' @param nRowsInHeader The number of rows that make up the header of the formatted Balance of Trade table
#' @keywords openxlsx
insertUpdatedSubTablesAsFormattedTable <- function(fileName, sheet, subTables, nRowsInHeader){

  # Calculate the number of rows and columns taken up by each sub table
  nRowsInAnnual <- nrow(subTables$Annually)
  nRowsInMonthly <- nrow(subTables$Monthly)
  nRows <- nRowsInAnnual + 2 + nRowsInMonthly
  nColumns <- ncol(subTables$Annually)
 
  # Calculate the start of each sub table
  annualStartRow <- nRowsInHeader + 1
  monthlyStartRow <- nRowsInHeader + nRowsInAnnual + 3
  notesStartRow <- monthlyStartRow + nRowsInMonthly + 1

  # Load the final tables workbook for editing
  finalWorkbook <- openxlsx::loadWorkbook(fileName)

  # Clear the current contents of the workbook
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=annualStartRow, x=matrix(NA, nrow=nRows+4, ncol=nColumns), colNames=FALSE)
  openxlsx::removeCellMerge(finalWorkbook, sheet=sheet, cols=seq_len(nColumns), rows=annualStartRow:notesStartRow)
  openxlsx::setRowHeights(finalWorkbook, sheet=sheet, rows=annualStartRow:notesStartRow, heights=14.5)

  # Insert the Annual sub table
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=annualStartRow, x=subTables$Annually, colNames=FALSE)

  # Insert the monthly sub table
  writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=monthlyStartRow-1, x="Monthly", colNames=FALSE)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=monthlyStartRow, x=subTables$Monthly, colNames=FALSE)

  # Insert the notes
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=notesStartRow, x=subTables$Notes, colNames=FALSE)

  # Apply a general formatting to the table contents
  default <- openxlsx::createStyle(borderStyle="thin", borderColour="black", border=c("top", "bottom", "left", "right"),
                                   fontName="Times New Roman")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=default, gridExpand=TRUE, cols=seq_len(nColumns), 
                     rows=annualStartRow:(notesStartRow+2), stack=FALSE)

  # Format the Monthly and Annually sub table names as bold
  bold <- openxlsx::createStyle(textDecoration="bold", halign="left")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=bold, rows=annualStartRow-1, cols=1, stack=TRUE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=bold, rows=monthlyStartRow-1, cols=1, stack=TRUE)

  # Format the balance of trade statistics as numbers
  numberWithComma <- openxlsx::createStyle(numFmt="#,##0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithComma, gridExpand=TRUE, stack=TRUE, cols=3:nColumns,
                     rows=annualStartRow:notesStartRow)

  # Format the year values in table as numbers
  numberWithoutComma <- openxlsx::createStyle(numFmt="0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithoutComma, stack=TRUE, cols=1, gridExpand=TRUE,
                     rows=annualStartRow:(monthlyStartRow-2))
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithoutComma, stack=TRUE, cols=1, gridExpand=TRUE,
                     rows=monthlyStartRow:notesStartRow)

  # Format the notes as italics
  italics <- openxlsx::createStyle(textDecoration="italic", halign="left", valign="bottom")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=italics, cols=1:nColumns, stack=TRUE, gridExpand=TRUE,
                     rows=notesStartRow:(notesStartRow+4))
  
  # Remove formatting outside the table region
  blank <- openxlsx::createStyle(borderStyle="none", border=c("top", "bottom", "left", "right"))
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=blank, gridExpand=TRUE, cols=(nColumns+1):100, 
                     rows=1:100, stack=FALSE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=blank, gridExpand=TRUE, cols=1:100, 
                     rows=(notesStartRow+nrow(subTables$Notes)):100, stack=FALSE)

  # Save the edited workbook as a new file
  openxlsx::saveWorkbook(finalWorkbook, file=fileName, overwrite=TRUE)
}

#' Inserts updated table (time as columns) back into formatted excel sheet
#' 
#' A function that inserts the updated Annual and Monthly statistics back into formatted Balance of Trade table 
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @param sheet A character string identifying the sheet to extract data from
#' @param table A structured data.frame updated to include the latest data
#' @param tableNumber The number of the table - reported in top left of formatted table in excel notebook
#' @param tableName The name of table (all caps) - second cell in top row 
#' @param boldRows A vector of numbers noting the rows in the formatted table that are formatted as bold
#' @param nRowsInNotes An integer indicating how many rows are in the notes section below the formatted table
#' @keywords openxlsx
insertUpdatedTableByCommodityAsFormattedTable <- function(fileName, sheet, table, tableNumber, tableName, boldRows, nRowsInNotes=4, numericColumns){
  
  # Get the column names and dimensions
  colNames <- colnames(table)
  nColumns <- length(colNames)
  nRows <- nrow(table)
  
  # Note the indices of the January monthly statistics
  januaryIndices <- which(grepl(colnames(table), pattern="Jan"))
  lastMonthyInYearIndices <- ifelse(januaryIndices+11 < nColumns, januaryIndices+11, nColumns)

  # Identify the column where the annual table ends
  lastAnnualColumn <- januaryIndices[1] - 1
  
  # Note the years monthly data available for
  years <- as.numeric(colNames[lastAnnualColumn]) - c((length(januaryIndices)-1):0)

  # Set class of numeric columns to numeric
  for(column in numericColumns){
    table[, column] <- as.numeric(table[, column])
  }

  # Load the final tables workbook for editing
  finalWorkbook <- openxlsx::loadWorkbook(fileName)
  
  # Clear the current contents of the workbook
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=2, startRow=1, x=matrix(NA, nrow=nRows+5, ncol=nColumns-1), colNames=FALSE)
  openxlsx::removeCellMerge(finalWorkbook, sheet=sheet, cols=2:nColumns, rows=1:(nRows+5))
  
  # Insert the updated table
  parsedColNames <- data.frame(matrix(nrow=1, ncol=length(colNames)))
  parsedColNames[1, ] <- sapply(colNames, 
                                FUN=function(colName){
                                  return(strsplit(colName, split="\\.")[[1]][1])
                                })
  for(column in numericColumns[1]:lastAnnualColumn){
    parsedColNames[, column] <- as.numeric(parsedColNames[, column])
  }
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=numericColumns[1], startRow=5, x=parsedColNames[, numericColumns], colNames=FALSE)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=6, x=table, colNames=FALSE)
  
  # Add in the header section
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=1, x=paste0("Table ", tableNumber), colNames=FALSE)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=2, startRow=1, x=tableName, colNames=FALSE)
  openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=2:nColumns, rows=1)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=numericColumns[1], startRow=2, x="[VT Million]", colNames=FALSE)
  openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=numericColumns[1]:nColumns, rows=2)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=numericColumns[1], startRow=3, x="ANNUALLY", colNames=FALSE)
  openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=numericColumns[1]:lastAnnualColumn, rows=3:4)
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=januaryIndices[1], startRow=3, x="MONTHLY", colNames=FALSE)
  openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=(lastAnnualColumn+1):nColumns, rows=3)
  
  for(index in seq_along(years)){
    openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=januaryIndices[index], startRow=4, x=years[index], colNames=FALSE)
    openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=januaryIndices[index]:lastMonthyInYearIndices[index], rows=4)
  }

  # Set the general style for the table
  defaultFormat <- openxlsx::createStyle(borderStyle="thin", borderColour="black", border=c("top", "bottom", "left", "right"),
                                   fontName="Times New Roman")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=defaultFormat, gridExpand=TRUE, cols=numericColumns, 
                     rows=1:(nRows+5+1+nRowsInNotes), stack=FALSE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=defaultFormat, gridExpand=TRUE, cols=seq_len(nColumns), 
                     rows=6:(nRows+5+1+nRowsInNotes), stack=FALSE)
  
  # Set the column widths
  openxlsx::setColWidths(finalWorkbook, sheet=sheet, cols=numericColumns[1]:lastAnnualColumn, width=7.5)
  openxlsx::setColWidths(finalWorkbook, sheet=sheet, cols=(lastAnnualColumn+1):nColumns, width=6.5)

  # Format the header region
  headerFormat <- openxlsx::createStyle(textDecoration="bold", halign="center")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=headerFormat, gridExpand=TRUE, cols=1:nColumns, rows=1:5, stack=TRUE)
  numberWithoutComma <- openxlsx::createStyle(numFmt="0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithoutComma, gridExpand=TRUE, cols=numericColumns[1]:lastAnnualColumn, rows=5,
                     stack=TRUE)
  tableNumberFormat <- openxlsx::createStyle(textDecoration="bold", halign="left")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=tableNumberFormat, cols=1, rows=1, stack=TRUE)
  
  # Format the numbers
  numberWithComma <- openxlsx::createStyle(numFmt="#,##0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithComma, gridExpand=TRUE, cols=2:nColumns, rows=6:(nRows+5), stack=TRUE)
  
  # Format the non-numeric columns
  nonNumericColumns <- seq_along(nColumns)[seq_along(nColumns) %in% numericColumns == FALSE]
  tableNumberFormat <- openxlsx::createStyle(halign="left", wrapText=TRUE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=tableNumberFormat, cols=nonNumericColumns, rows=6:nRows, stack=TRUE)
  
  # Format the bold rows
  bold <- openxlsx::createStyle(textDecoration="bold")
  for(row in boldRows){
    openxlsx::addStyle(finalWorkbook, sheet=sheet, style=bold, gridExpand=TRUE, cols=1:nColumns, rows=row, stack=TRUE)
  }
  
  # Remove formatting outside the table region
  blank <- openxlsx::createStyle(borderStyle="none", border=c("top", "bottom", "left", "right"))
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=blank, gridExpand=TRUE, cols=(nColumns+1):100, 
                     rows=1:100, stack=FALSE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=blank, gridExpand=TRUE, cols=1:100, 
                     rows=(nRows+5+1+nRowsInNotes+1):100, stack=FALSE)
  
  # Save the edited workbook as a new file
  openxlsx::saveWorkbook(finalWorkbook, file=fileName, overwrite=TRUE)
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
