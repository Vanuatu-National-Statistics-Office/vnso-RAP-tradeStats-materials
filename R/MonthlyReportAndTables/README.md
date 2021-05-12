# Example workflow for monthly trade statistics reporting

## Processing the raw statistics data
1. Save raw trade statistics data in `data/secure/` folder in `CSV` (`C`omma `S`eparated `V`alues) format
2. Open [`R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R) in RStudio
3. Update `line 24` with the name of the raw trade statistics data:
    https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/333325dc5ce95e5269865a2b52b48a58f19fd516/R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R#L24
4. Click *Source* in the top right corner

## Updating the monthly report tables
5. Save the excel formatted monthly report tables file (with data up until the previous month already present) in the  `outputs/` folder
6. Open [`R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/1_processLatestMonthsTradeStats.R) in RStudio
7. Update `line 21` with the name of the formatted monthly report file:
    https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/333325dc5ce95e5269865a2b52b48a58f19fd516/R/MonthlyReportAndTables/2_insertLatestMonthsDataIntoFormattedTables.R#L21
8. Click *Source* in the top right corner

## Generating the monthly report

9. Open [`R/MonthlyReportAndTables/3_buildMonthlyReport.Rmd`](https://github.com/Vanuatu-National-Statistics-Office/vnso-RAP-tradeStats-materials/blob/automated-highlights-report-new/R/MonthlyReportAndTables/3_buildMonthlyReport.Rmd) in RStudio
10. Click *Knit* in the top left corner