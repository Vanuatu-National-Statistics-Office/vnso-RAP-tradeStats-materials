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
#' @keywords BoT openxlsx
extractHistoricBalanceOfTradeStatistics <- function(fileName){
  
  # Extract the full historic table
  historic <- openxlsx::read.xlsx(fileName, sheet="1_BOT", skipEmptyRows=TRUE, startRow=5)
  
  # Identify when each sub-table starts
  annualStart <- 1
  monthlyStart <- which(historic[, 1] == "Monthly")
  end <- which(historic[, 1] == "Notes:") - 1
  
  # Extract the Annual statistics
  annually <- historic[1:(monthlyStart-1), ]
  colnames(annually) <- c("Year", "blank", "Exports", "Re-exports", "Total", "Imports-CIF", "Balance")
  
  # Extract the monthly statistics
  monthly <- historic[(monthlyStart+1):end, ]
  colnames(monthly) <- c("Year", "Month", "Exports", "Re-exports", "Total", "Imports-CIF", "Balance")
  
  # Return the extract sub tables
  return(list("Annually"=annually, "Monthly"=monthly))
}