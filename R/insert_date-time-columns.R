# insertLocalDateTimeColumns ---------------------------------------------------

#' Insert LocalDateTime Columns
#' 
#' @param mydata data frame with character column \emph{BerlinDateTimeNoDST}
#' 
#' @return data frame with additional columns \emph{BerlinDateTime} (character),
#'   \emph{UTCOffset} (numeric)
#' 
#' @export
#' 
insertLocalDateTimeColumns <- function(mydata)
{
  colname <- "BerlinDateTimeNoDST"
  winterTimeCol <- which(names(mydata) == colname)
  
  if (length(winterTimeCol) != 1) {
    
    stop(sprintf("No column \"%s\" found in data frame \"mydata\"!", colname))
  }
  
  winter_times <- mydata[[winterTimeCol]]
  
  berlinTime <- kwb.datetime::berlinNormalTimeToBerlinLocalTime(winter_times)
  utcTime <- kwb.datetime::berlinNormalTimeToUTC(winter_times)
  
  timesOnly <- kwb.utils::noFactorDataFrame(
    BerlinDateTimeNoDST = winter_times,
    BerlinDateTime = berlinTime,
    UTCOffset = kwb.datetime::utcOffset(berlinTime, utcTime)
  )
  
  cbind(timesOnly, mydata[, -winterTimeCol, drop = FALSE])
}

# insertUtcDateTimeColumn ------------------------------------------------------

#' Insert DateTimeUTC Column
#' 
#' @param mydata data frame with column \code{BerlinDateTimeNoDST}
#' 
#' @return mydata with additional column \emph{DateTimeUTC}
#' 
#' @export
#' 
insertUtcDateTimeColumn <- function(mydata)
{
  utcTime <- kwb.datetime::berlinNormalTimeToUTC(mydata$BerlinDateTimeNoDST)
  
  cbind(mydata, DateTimeUTC = utcTime, stringsAsFactors = FALSE)
}

# completeTimeColumns ----------------------------------------------------------

#' Complete Time Columns
#' 
#' @param x data frame with time columns
#' @param wanted Default: c("BerlinDateTime", "UTCOffset", "DateTimeUTC")
#' 
#' @return (Hopefully) data frame with columns \emph{BerlinDateTimeNoDST},
#'   \emph{BerlinDateTime}, \emph{UTCOffset}, \emph{DateTimeUTC},
#' 
completeTimeColumns <- function(
  x, wanted = c("BerlinDateTime", "UTCOffset", "DateTimeUTC")
)
{
  cnames <- names(x)
  berlinTime <- NULL
  
  if (wantedButNotAvailable("BerlinDateTime", wanted, cnames)) {
    
    if ("BerlinDateTimeNoDST" %in% cnames) {
      
      x$BerlinDateTime <- kwb.datetime::berlinNormalTimeToBerlinLocalTime(
        as.character(x$BerlinDateTimeNoDST)
      )
    }
  }
  
  if (wantedButNotAvailable("UTCOffset", wanted, cnames)) {
    
    if (is.null(berlinTime) && "BerlinDateTimeNoDST" %in% cnames) {
      
      bnt <- as.character(x$BerlinDateTimeNoDST)
      
      x$UTCOffset <- kwb.datetime::utcOffset(
        kwb.datetime::berlinNormalTimeToBerlinLocalTime(bnt),
        kwb.datetime::berlinNormalTimeToUTC(bnt)
      )
    }
  }
  
  if (wantedButNotAvailable("DateTimeUTC", wanted, cnames)) {
    
    if ("BerlinDateTimeNoDST" %in% cnames) {
      
      x$DateTimeUTC <- kwb.datetime::berlinNormalTimeToUTC(
        as.character(x$BerlinDateTimeNoDST)
      )
    }
  }
  x
}

# wantedButNotAvailable --------------------------------------------------------
wantedButNotAvailable <- function(cname, wanted, available)
{
  cname %in% wanted & !(cname %in% available)
}
