# Example workflow for monthly trade statistics reporting

## Processing the raw statistics data
1. Save raw trade statistics data in `data/secure/` folder in `CSV` (`C`omma `S`eparated `V`alues) format
2. Open [`R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R) in RStudio
3. Update `line 24` with the name of the raw trade statistics data
4. Click <img src="https://user-images.githubusercontent.com/17436210/117978667-093e2c80-b32a-11eb-996a-37ab7f659ad8.png" width="20">
 *Source* in the top right corner

## Updating the monthly report tables
5. Save the excel formatted monthly report tables file (with data up until the previous month already present) in the  `outputs/` folder
6. Open [`R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R) in RStudio
7. Update `line 21` with the name of the formatted monthly report file
8. Click <img src="https://user-images.githubusercontent.com/17436210/117978683-0d6a4a00-b32a-11eb-8c62-d00257d3b282.png" width="20">
 *Source* in the top right corner

## Generating the monthly report

9. Open [`R/MonthlyReportAndTables/3_buildMonthlyReport.Rmd`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/3_buildMonthlyReport.Rmd) in RStudio
10. Click <img src="https://user-images.githubusercontent.com/17436210/117978613-faf01080-b329-11eb-80e0-8a062742c8ab.png" width="20">
*Knit* in the top left corner
