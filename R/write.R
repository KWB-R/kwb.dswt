# writeHQSeriesToCSV -----------------------------------------------------------

#' Write H-Q-Series to CSV File
#' 
#' @param hqSeries data frame containing HQ time series
#' @param csv full path to csv file
#' @param sep column separator. Default: ";"
#' @param dec decimal character. Default: "."
#' 
#' @export
#' 
writeHQSeriesToCSV <- function(hqSeries, csv, sep = ";", dec = ",")
{
  cat("*** Writing HQ time series to", kwb.utils::windowsPath(csv), "... ")
  utils::write.table(
    hqSeries, csv, row.names = FALSE, sep = sep, dec = dec, na = ""
  )
  cat("ok.\n")
}

# writeEventListToCSV ----------------------------------------------------------

#' Write Event List to CSV File
#' 
#' @param events data frame containing event data
#' @param csv full path to csv file
#' @param sep column separator. Default: ";"
#' @param dec decimal character. Default: "."
#' 
#' @export
#' 
writeEventListToCSV <- function(events, csv, sep = ";", dec = ",")
{
  cat("*** Writing event list to", kwb.utils::windowsPath(csv), "... ")
  utils::write.table(
    events, csv, row.names = FALSE, sep = sep, dec = dec, na = ""
  )
  cat("ok.\n")
}
