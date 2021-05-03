
#' Pad the SITC or HS code values with zeros
#'
#' Pads SITC or HS values with leading/lagging zeros to conform with classification standards. Add zeros to left of HS code to make them 8 digits. Adds zeros to SITC codes to match 5 digit format 000.00.
#' @param code A value representing an HS or SITC code
#' @param type The type of codes ("SITC" or "HS") in values. Defaults to "HS"
#' @return A character string of the code padded with zeros
padWithZeros <- function(code, type = "HS"){
  
  # Skip NA values
  if(is.na(code)){
    return(code)
  }
  
  # Convert the code to a character string
  code <- as.character(code)

  # Handle HS codes
  if(type == "HS"){
    
    code <- paste0(paste(rep(0, 8 - nchar(code)), collapse = ""), code)

  # Handle SITC codes
  }else if(type == "SITC"){

    partsOfCode <- strsplit(code, split = "\\.")[[1]]

    if(length(partsOfCode) == 2 && nchar(partsOfCode) <= 3 && nchar(partsOfCode[2]) <= 2){
      code <- paste0(paste(rep(0, 3 - nchar(partsOfCode[1])), collapse = ""), partsOfCode[1], ".", 
                            partsOfCode[2], paste0(rep(0, 3 - nchar(partsOfCode[1])), collapse = ""))
    }else{
      warning(paste0("Format of current SITC code (", code, ") isn't in the expected form: 000.00. Code will remain unchanged."))
    }
    
  }else{
    warning(paste0("Code type provided (", type, ") is not recognised. Should be one of c(\"SITC\" or \"HS\"). Code will remain unchanged."))
  }
  
  return(code)
}

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
extractSubTablesFromFormattedTableByTime <- function(fileName, sheet, startRow, nColumns,
                                                     numericColumns=c(1,3:nColumns)){
  
  # Extract the full historic table
  historic <- openxlsx::read.xlsx(fileName, sheet=sheet, skipEmptyRows=FALSE, startRow=startRow)
  if(is.null(nColumns) == FALSE){
    historic <- historic[, seq_len(nColumns)]
  }
  
  # Identify when each sub-table starts
  annualStart <- 1
  monthlyStart <- which(tolower(historic[, 1]) == "monthly")
  end <- which(historic[, 1] == "Notes:") - 1
  
  # Extract the Annual statistics
  annually <- historic[1:(monthlyStart-2), ]
  colnames(annually)[c(1,2)] <- c("Year", "blank")
  for(columnIndex in c(1, 3:ncol(annually))){
    annually[, columnIndex] <- as.numeric(annually[, columnIndex])
  }

  # Extract the monthly statistics
  monthly <- historic[(monthlyStart+1):end, ]
  colnames(monthly)[c(1,2)] <- c("Year", "Month")
  for(columnIndex in numericColumns){
    monthly[, columnIndex] <- as.numeric(monthly[, columnIndex])
  }

  # Extract the notes from the table
  notes <- historic[(end+1):nrow(historic), c(1,2)]
  colnames(notes) <- c("Notes", "blank")
  
  # Return the extract sub tables
  return(list("Annually"=annually, "Monthly"=monthly[rowSums(is.na(monthly)) != ncol(monthly), ], "Notes"=notes))
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
updateSubTablesByTime <- function(subTables, month, year, newStatistics, monthColumn="Month", numericColumns=3:ncol(subTables$Annually)){

  # Check if data have already been inserted into sub tables - suppressing warnings here as could be co-ercing months into numbers (Table 10 for example)
  latestYearInSubTables <- max(suppressWarnings(as.numeric(subTables$Annually$Year)), na.rm=TRUE)
  months <- subTables$Monthly[, monthColumn][is.na(subTables$Monthly[, monthColumn]) == FALSE]
  latestMonth <- months[length(months)]
  if(latestYearInSubTables == (as.numeric(year)-1) && latestMonth == month || latestYearInSubTables == as.numeric(year)){
    warning("Sub tables already contain data for specified month.")
    return(NULL)
  }

  # Note whether new statistics are split by imports and exports
  twoRows <- nrow(newStatistics) == 2
  
  # Check if we need to update the annual table
  if(month == "December"){
    
    # Identify the rows for last year
    januaryRows <- grep(subTables$Monthly[, monthColumn], pattern="Jan")
    lastYearsRows <- januaryRows[length(januaryRows)]:nrow(subTables$Monthly)
    
    # Initialise a variables to stroe last year's totals
    currentYearsTotals <- data.frame("Year"=as.numeric(year), "blank"=NA, stringsAsFactors=FALSE)
    
    # Check if tables inlcude both Exports and Imports data
    if(twoRows){
      currentYearsTotals[1, colnames(subTables$Annually)[numericColumns]] <- 
        colSums(subTables$Monthly[lastYearsRows[seq(from=1, to=length(lastYearsRows)-1, by=2)], numericColumns], na.rm=TRUE) + newStatistics[1, ]
      currentYearsTotals[2, colnames(subTables$Annually)[numericColumns]] <- 
        colSums(subTables$Monthly[lastYearsRows[seq(from=2, to=length(lastYearsRows), by=2)], numericColumns], na.rm=TRUE) + newStatistics[2, ]
      currentYearsTotals$blank <- c("Exports", "Imports")
    }else{
      currentYearsTotals[, colnames(subTables$Annually)[numericColumns]] <- colSums(subTables$Monthly[lastYearsRows, numericColumns], na.rm=TRUE) + newStatistics
    }
    
    # Add a new totals to the annual table for this years data
    subTables$Annually <- rbind(subTables$Annually, currentYearsTotals)
  }
  
  # Change the column names of the new statistics to match the sub tables
  names(newStatistics) <- colnames(subTables$Monthly)[numericColumns]
  
  # Add a year column to new Statistics table
  newStatistics$Year <- NA
  
  # Add in the current month
  if(twoRows){
    newStatistics[1, "Year"] <- month
    newStatistics[, "Month"] <- c("Exports", "Imports")
  }else{
    newStatistics[1, monthColumn] <- month
  }
  
  # If the new statistics are for January add the current year into the table
  if(month == "January" && twoRows){
    newStatistics <- rbind(NA, newStatistics)
    newStatistics[1, "Year"] <- as.numeric(year)
  }else if(month == "January"){
    newStatistics$Year <- as.numeric(year)
  }
  
  # Add an empty row if December
  if(month == "December"){
    newStatistics <- rbind(newStatistics, NA)
  }
  
  # Update the monthly table
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
  if((as.numeric(year) - 1) %in% colNames && grepl(colNames[length(colNames)], pattern=substr(month, 1, 3))){
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
  
  # Check if December - then the annual section of table needs to be updated as well
  if(month == "December"){
    
    # Calculate the sum for each commodity over the previous year
    naCounts <- unlist(apply(structuredTable[, (nColumns - 11):nColumns], MARGIN=1,
                             FUN=function(values){
                                return(sum(is.na(values)))
                             }))
    previousYearsTotals <- rowSums(structuredTable[, (nColumns - 11):nColumns], na.rm=TRUE)
    previousYearsTotals[naCounts == 12] <- NA # Not calculating row sum for empty rows in table

    # Identify the column where the annual table ends
    lastAnnualColumn <- grep(colnames(structuredTable), pattern="Jan")[1] - 1
    
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
#' @param nRowsInNotes An integer indicating how many rows are in the notes section below the formatted table
#' @param loadAndSave Boolean value indicating whether to load the full excel file and save any changes (default) or to make changes to an already loaded file
#' @param finalWorkbook A loaded excel file, used if \code{loadAndSave == FALSE}
#' @keywords openxlsx
insertUpdatedSubTablesAsFormattedTable <- function(fileName, sheet, subTables, nRowsInHeader, loadAndSave=TRUE, finalWorkbook=NULL){

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
  if(loadAndSave){
    finalWorkbook <- openxlsx::loadWorkbook(fileName)
  }

  # Clear the current contents of the workbook
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=1, startRow=annualStartRow, x=matrix(NA, nrow=nRows+1+nrow(subTables$Notes), ncol=nColumns), colNames=FALSE)
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
                     rows=annualStartRow:(notesStartRow+3), stack=FALSE)

  # Format the Monthly and Annually sub table names as bold
  bold <- openxlsx::createStyle(textDecoration="bold", halign="left")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=bold, rows=annualStartRow-1, cols=1, stack=TRUE)
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=bold, rows=monthlyStartRow-1, cols=1, stack=TRUE)

  # Format the balance of trade statistics as numbers
  numberWithComma <- openxlsx::createStyle(numFmt="#,##0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithComma, gridExpand=TRUE, stack=TRUE, cols=3:nColumns,
                     rows=annualStartRow:notesStartRow)

  # Format the year values in table as numbers
  numberWithoutComma <- openxlsx::createStyle(numFmt="GENERAL")
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
  if(loadAndSave){
    openxlsx::saveWorkbook(finalWorkbook, file=fileName, overwrite=TRUE)
  }else{
    return(finalWorkbook)
  }
}

#' Inserts updated table (time as columns) back into formatted excel sheet
#' 
#' A function that inserts the updated Annual and Monthly statistics back into formatted Balance of Trade table 
#' @param fileName A character string of full path for formatted trade statistics tables in excel workbook
#' @param sheet A character string identifying the sheet to extract data from
#' @param table A structured data.frame updated to include the latest data
#' @param year A character vector indicating the year of new statistics to be added
#' @param tableNumber The number of the table - reported in top left of formatted table in excel notebook
#' @param tableName The name of table (all caps) - second cell in top row 
#' @param boldRows A vector of numbers noting the rows in the formatted table that are formatted as bold
#' @param nRowsInNotes An integer indicating how many rows are in the notes section below the formatted table
#' @param loadAndSave Boolean value indicating whether to load the full excel file and save any changes (default) or to make changes to an already loaded file
#' @param finalWorkbook A loaded excel file, used if \code{loadAndSave == FALSE}
#' @keywords openxlsx
insertUpdatedTableByCommodityAsFormattedTable <- function(fileName, sheet, table, year, tableNumber, tableName, boldRows, 
                                                          nRowsInNotes=4, numericColumns, loadAndSave=TRUE, finalWorkbook=NULL){
  
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
  years <- as.numeric(year) - c((length(januaryIndices)-1):0)

  # Set class of numeric columns to numeric
  for(column in numericColumns){
    table[, column] <- as.numeric(table[, column])
  }

  # Load the final tables workbook for editing
  if(loadAndSave){
    finalWorkbook <- openxlsx::loadWorkbook(fileName)
  }
  
  # Clear the current contents of the workbook
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=numericColumns[1], startRow=1, x=matrix(NA, nrow=nRows+5, ncol=nColumns-numericColumns[1]), colNames=FALSE)
  openxlsx::removeCellMerge(finalWorkbook, sheet=sheet, cols=numericColumns[1]:nColumns, rows=1:(nRows+5))
  
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
  openxlsx::writeData(finalWorkbook, sheet=sheet, startCol=numericColumns[1], startRow=1, x=tableName, colNames=FALSE)
  openxlsx::mergeCells(finalWorkbook, sheet=sheet, cols=numericColumns[1]:nColumns, rows=1)
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
  numberWithoutComma <- openxlsx::createStyle(numFmt="GENERAL")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithoutComma, gridExpand=TRUE, cols=numericColumns[1]:lastAnnualColumn, rows=5,
                     stack=TRUE)
  tableNumberFormat <- openxlsx::createStyle(textDecoration="bold", halign="left")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=tableNumberFormat, cols=1, rows=1, stack=TRUE)
  
  # Format the numbers
  numberWithComma <- openxlsx::createStyle(numFmt="#,##0")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=numberWithComma, gridExpand=TRUE, cols=2:nColumns, rows=6:(nRows+5), stack=TRUE)
  
  # Format the non-numeric columns
  nonNumericColumns <- seq_len(numericColumns[1]-1)
  tableNumberFormat <- openxlsx::createStyle(halign="left", wrapText=TRUE, borderStyle="thin", borderColour="black", border=c("top", "bottom", "left", "right"),
                                             fontName="Times New Roman")
  openxlsx::addStyle(finalWorkbook, sheet=sheet, style=tableNumberFormat, gridExpand=TRUE, cols=nonNumericColumns, rows=6:(nRows+5), stack=FALSE)
  
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
  if(loadAndSave){
    openxlsx::saveWorkbook(finalWorkbook, file=fileName, overwrite=TRUE)
  }else{
    return(finalWorkbook)
  }
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
                             
                             # Check for NA values
                             if(is.na(code)){
                               warning(paste0("Skipping NA value when extract subset of code."))
                               return(NA)
                             }
                             
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
#' @param by A vector of columns used as common identifiers in merge operation. Note where multiple values are present, each should have corresponding value in \code{column}.
#' @param columns A vector of columns that were pulled in during merge
#' @param printWarnings Boolean value to indicate whether or not to print warnings
searchForMissingObservations <- function(merged, by, columns, printWarnings=TRUE){
  
  # Check that by and columns are the same length
  if(length(by) != length(columns)){
    stop("Number of values in \"by\" and \"columns\" parameters don't match. For each column provided in \"by\" parameter a paired column (that was pulled in during merge) should be provided.")
  }
  
  # Initialise a data.frame to store information about the missing observations
  missingInfo <- data.frame("ClassificationValue"=NA, "ClassificationColumn"=NA, "MergedColumn"=NA)
  row <- 0
  
  # Examine each of the columns of interest
  for(columnIndex in seq_along(columns)){
    
    # Skip if column names provided for by and matched columns are not present
    if(columns[columnIndex] %in% colnames(merged) == FALSE){
      stop(paste0("Column name provided in \"columns\" parameter (", columns[columnIndex], ") not present in table"))
    }
    if(by[columnIndex] %in% colnames(merged) == FALSE){
      stop(paste0("Column name provided in \"by\" parameter (", by[columnIndex], ") not present in table"))
    }
    
    # Check if any NA values are present in column of interest
    naIndices <- which(is.na(merged[, columns[columnIndex]]))
    
    # If NAs are present report the category they are present for
    for(naIndex in naIndices){
      
      # Store information about the current missing observation
      row <- row + 1
      missingInfo[row, ] <- c(merged[naIndex, by[columnIndex]], by[columnIndex], columns[columnIndex])
      
      # Print warning if requested
      if(printWarnings){
        warning(paste0("No observation present in classification table for \"", merged[naIndex, by[columnIndex]]), "\" in ", by[columnIndex], "\n")
      }
    }
  }
  
  if(row == 0){
    return(NULL)
  }else{
    return(missingInfo)
  }
}

#' Remove empty columns from the end of table
#' 
#' A function that will remove columns that are empty (all NA values) - often happens when importing table from excel
#' @param table A data.frame 
removeEmptyColumnsAtEnd <- function(table){
  
  # Get the column names
  colNames <- colnames(table)
  
  # Identify the non-labelled columns
  nonLabelledColumns <- grep(colNames, pattern="^X")
  
  # If no unlabelled coklumns found finish
  if(length(nonLabelledColumns) == 0){
    return(table)
  }
  
  # Identify last monthly column
  monthlyColumns <- which(colNames %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  # Identify non-labelled columns after last monthly column
  nonLabelledColumnsToRemove <- nonLabelledColumns[nonLabelledColumns > monthlyColumns[length(monthlyColumns)]]
  
  # Remove unlablled columns after last monthly column if found
  if(length(nonLabelledColumnsToRemove) > 0){
    
    table <- table[, -nonLabelledColumnsToRemove]
  }
  
  return(table)
}

#' Calculate range of summary statistics for distribution 
#' 
#' For values provided, calculates mean, median, standard deviation, 95\\% percentiles, count, range and number of NA values 
#' @param values A numeric vector of values 
#' @return Returns an numeric labeled vector containing the summary statistics
calculateSummaryStatistics <- function(values){
  
  # Create an output vector to store the summary statistics
  output <- c("Mean"=NA, "SD"=NA, "Median"=NA, "Lower-2.5"=NA, "Upper-97.5"=NA,
              "Lower-1"=NA, "Upper-99"=NA,
              "Min"=NA, "Max"=NA, "Count"=NA, "CountMissing"=NA)
  
  # Count number of values and any missing data
  output["Count"] <- length(values)
  output["CountMissing"] <- sum(is.na(values) | is.infinite(values) | is.nan(values))
  
  # Check if only missing available - if so stop and return empty summary statistics
  if(output["CountMissing"] == output["Count"]){
    return(output)
  }
  
  # Calculate mean
  output["Mean"] <- mean(values, na.rm=TRUE)
  
  # Calculate standard deviation
  output["SD"] <- sd(values, na.rm=TRUE)
  
  # Calculate median
  output["Median"] <- median(values, na.rm=TRUE)
  
  # Calculate upper and lower 95% percentile bounds
  quantiles <- quantile(values, probs=c(0.99, 0.975, 0.025, 0.01), na.rm=TRUE)
  output["Upper-99"] <- quantiles[1]
  output["Upper-97.5"] <- quantiles[2]
  output["Lower-2.5"] <- quantiles[3]
  output["Lower-1"] <- quantiles[4]
  
  # Calculate the range of the data
  minMax <- range(values, na.rm=TRUE)
  output["Min"] <- minMax[1]
  output["Max"] <- minMax[2]
  
  return(output)
}

#' Check whether commodity values fall outside of expectations base don historic data 
#' 
#' The range, 95% and 99% bounds were calculated using historic data. The current function checks the values of commodities in the latest month to see whether any fall outside of the historic range of 95% and 99% bounds. 
#' @param tradeStats The process trade statistic values. Must include HS.Code, CP4 and Stat..Value columns.
#' @param historicImportsSummaryStats Range of summary statistics generated for the Value and Unit Value of historic imported commodities. Must include HS, Value|UnitValue: "Median", "Lower.2.5", "Upper.97.5", "Lower.1", "Upper.99", "Min", "Max"
#' @param historicExportsSummaryStats Range of summary statistics generated for the Value and Unit Value of historic exported commodities. Must include HS, Value|UnitValue: "Median", "Lower.2.5", "Upper.97.5", "Lower.1", "Upper.99", "Min", "Max"
#' @param importCP4s CP4 codes that identify IMPORTS
#' @param exportCP4s CP4 codes that identify EXPORTS
#' @param useUnitValue Boolean value indicating whether to use range, 95% and 99% bounds of the Unit Value. Defaults to false and uses Value.
#' @param columsnOfInterest A vector of columns of interest to include in summary statistics table (merge by) to go alongside "Stat..Value" column
#' @return Returns an numeric labeled vector containing the summary statistics
checkCommodityValues <- function(tradeStats, historicImportsSummaryStats, historicExportsSummaryStats,
                                 importCP4s=c(4000, 4071, 7100), exportCP4s=c(1000), useUnitValue=FALSE,
                                 columnsOfInterest=c("HS.Code", "Type", "Reg..Date", 
                                                     "CP4", "Itm..")){
  
  # Pad the HS codes with zeros to make up to 8 digits
  padHSCode <- function(hsCode, length=8){
    return(paste0(paste(rep(0, length-nchar(hsCode)), collapse=""), hsCode))
  }
  historicImportsSummaryStats$HS <- sapply(historicImportsSummaryStats$HS, FUN=padHSCode)
  historicExportsSummaryStats$HS <- sapply(historicExportsSummaryStats$HS, FUN=padHSCode)
  
  # Combine the summary stats tables
  historicImportsSummaryStats$HSAndType <- paste0("IMPORT_", historicImportsSummaryStats$HS)
  historicExportsSummaryStats$HSAndType <- paste0("EXPORT_", historicExportsSummaryStats$HS)
  historicSummaryStats <- rbind(historicImportsSummaryStats, historicExportsSummaryStats)
  
  # Identify imports and exports and create
  tradeStats$Type <- "UNKNOWN"
  tradeStats$Type[tradeStats$CP4 %in% importCP4s] <- "IMPORT"
  tradeStats$Type[tradeStats$CP4 %in% exportCP4s] <- "EXPORT"
  tradeStats$HSAndType <- paste0(tradeStats$Type, "_", tradeStats$HS.Code)
  
  # Note whether using unit value
  summaryPrefix <- "Value."
  if(useUnitValue){
    summaryPrefix <- "UnitValue."
  }
  summaryColumnsOfInterest <- paste0(summaryPrefix, c("Median", "Lower.2.5", "Upper.97.5", "Lower.1", "Upper.99", "Min", "Max"))
  
  # Get the expected distribution summary statistics for each commodity
  commoditiesWithExpectations <- merge(tradeStats[, c("HSAndType", columnsOfInterest, "Stat..Value")],
                                       historicSummaryStats[, c("HSAndType", summaryColumnsOfInterest)],
                                       by="HSAndType",
                                       all.x=TRUE)
  
  # Check the values in latest month are within expected boundaries
  commoditiesWithExpectations$within95Bounds <- 
    commoditiesWithExpectations$Stat..Value >= commoditiesWithExpectations[, paste0(summaryPrefix, "Lower.2.5")] &
    commoditiesWithExpectations$Stat..Value <= commoditiesWithExpectations[, paste0(summaryPrefix, "Upper.97.5")]
  commoditiesWithExpectations$within99Bounds <- 
    commoditiesWithExpectations$Stat..Value >= commoditiesWithExpectations[, paste0(summaryPrefix, "Lower.1")] &
    commoditiesWithExpectations$Stat..Value <= commoditiesWithExpectations[, paste0(summaryPrefix, "Upper.99")]
  commoditiesWithExpectations$withinRange <- 
    commoditiesWithExpectations$Stat..Value >= commoditiesWithExpectations[, paste0(summaryPrefix, "Min")] &
    commoditiesWithExpectations$Stat..Value <= commoditiesWithExpectations[, paste0(summaryPrefix, "Max")]
  
  # Report whether values are within expectations
  boundariesNotAvailable <- which(is.na(commoditiesWithExpectations[, paste0(summaryPrefix, "Median")]))
  if(length(boundariesNotAvailable) > 0){
    warning(paste0(length(boundariesNotAvailable), " HS codes were not found in historic distribution summaries. Investigate these commodities further with the following code:\n\tView(commoditiesWithExpectations[is.na(commoditiesWithExpectations$withinRange), ])"))
  }
  notWithinPreviouslyObservedRange <- which(is.na(commoditiesWithExpectations$withinRange) == FALSE & commoditiesWithExpectations$withinRange == FALSE)
  if(length(notWithinPreviouslyObservedRange) > 0){
    warning(paste0(length(notWithinPreviouslyObservedRange), " Statistical values were not within the previously observed range. Investigate these commodities further with the following code:\n\tView(commoditiesWithExpectations[is.na(commoditiesWithExpectations$withinRange) == FALSE & commoditiesWithExpectations$withinRange == FALSE, ])"))
  }
  notWithin99Bounds <- which(is.na(commoditiesWithExpectations$within99Bounds) == FALSE & commoditiesWithExpectations$within99Bounds == FALSE)
  if(length(notWithin99Bounds) > 0){
    warning(paste0(length(notWithin99Bounds), " Statistical values were not within the 99% bounds of the previously observed range. Investigate these commodities further with the following code:\n\tView(commoditiesWithExpectations[is.na(commoditiesWithExpectations$within99Bounds) == FALSE & commoditiesWithExpectations$within99Bounds == FALSE, ])"))
  }
  notWithin95Bounds <- which(is.na(commoditiesWithExpectations$within95Bounds) == FALSE & commoditiesWithExpectations$within95Bounds == FALSE)
  if(length(notWithin95Bounds) > 0){
    warning(paste0(length(notWithin95Bounds), " Statistical values were not within the 95% bounds of the previously observed range. Investigate these commodities further with the following code:\n\tView(commoditiesWithExpectations[is.na(commoditiesWithExpectations$within95Bounds) == FALSE & commoditiesWithExpectations$within95Bounds == FALSE, ])"))
  }
  
  return(commoditiesWithExpectations)
}
