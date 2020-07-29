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
  
  # Extract the monthly statistics
  monthly <- historic[(monthlyStart+1):end, ]
  colnames(monthly) <- c("Year", "Month", "Export", "Re-Export", "Total Export", "Imports CIF", "Trade Balance")
  monthly$`Total Export` <- as.numeric(monthly$`Total Export`)
  
  
  
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
  newTradeBalanceStatistics$Year <- ifelse(month == "January", year, NA)
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

  # Load the final tables workbook for editing
  finalWorkbook <- loadWorkbook(fileName)

  # Insert the Annual sub table
  numericColumns <- which(sapply(subTables$Annually, class) == "numeric")
  subTables$Annually[, numericColumns] <- round(subTables$Annually[, numericColumns], digits=0)
  writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader, x=subTables$Annually, colNames=FALSE)
  nRowsInAnnual <- nrow(subTables$Annually)
  
  # Insert the monthly sub table
  writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRowsInAnnual+1, x="Monthly", colNames=FALSE)
  subTables$Monthly[, numericColumns] <- round(subTables$Monthly[, numericColumns], digits=0)
  writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRowsInAnnual+2, x=subTables$Monthly, colNames=FALSE)
  nRowsInMonthly <- nrow(subTables$Monthly)
  
  # Insert the notes
  writeData(finalWorkbook, sheet="1_BOT", startCol=1, startRow=nRowsInHeader+nRowsInAnnual+2+nRowsInMonthly, x=rbind(NA, subTables$Notes), colNames=FALSE)
  
  # Save the edited workbook as a new file
  editedFinalWorkbookFileName <- gsub(pattern=".xlsx", replacement="_EDITED.xlsx", fileName)
  saveWorkbook(finalWorkbook, file=editedFinalWorkbookFileName, overwrite=TRUE)
}

#' Formats numbers to have comma every three digits
#' 
#' Converts number to string but inserts comma every three digits to mark thousands 
#' @param number A number to be formatted as a string
#' @keywords string number comma
formatNumberWithCommas <- function(number){
  
  # Split the number into its characters
  characters <- strsplit(as.character(number), split="")[[1]]
  
  # Remove negative sign, if present
  negativeSign <- ""
  if(is.na(characters) == FALSE && length(characters) > 0 && characters[1] == "-"){
    characters <- characters[-1]
    negativeSign <- "-"
  }
  
  # Initialise a string to store formatted number
  formatted <- ""
  
  # Examine each character
  for(i in length(characters):1){
    
    # Add the current character to formatted number
    formatted <- paste0(characters[i], formatted)
    
    # Insert a comma every three characters 
    if((length(characters) - (i - 1)) %% 3 == 0 && i != 1){
      formatted <- paste0(",", formatted)
    }
  }
  
  # Add negative sign back on if it was present
  formatted <- paste0(negativeSign, formatted)
  
  return(formatted)
}