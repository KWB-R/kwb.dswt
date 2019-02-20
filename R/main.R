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

  if (.wantedButNotAvailable("BerlinDateTime", wanted, cnames)) {
    if ("BerlinDateTimeNoDST" %in% cnames) {
      #berlinTime <- berlinWinterTimeToBerlinLocalTime(as.character(x$BerlinDateTimeNoDST))
      #x$BerlinDateTime <- berlinTime$charLocal
      x$BerlinDateTime <- kwb.datetime::berlinNormalTimeToBerlinLocalTime(as.character(x$BerlinDateTimeNoDST))
    }
  }

  if (.wantedButNotAvailable("UTCOffset", wanted, cnames)) {
    if (is.null(berlinTime) && "BerlinDateTimeNoDST" %in% cnames) {
      #berlinTime <- berlinWinterTimeToBerlinLocalTime(as.character(x$BerlinDateTimeNoDST))
      #x$UTCOffset <- berlinTime$utcOffset
      bnt <- as.character(x$BerlinDateTimeNoDST)
      x$UTCOffset <- kwb.datetime::utcOffset(
        kwb.datetime::berlinNormalTimeToBerlinLocalTime(bnt),
        kwb.datetime::berlinNormalTimeToUTC(bnt)
      )
    }
  }

  if (.wantedButNotAvailable("DateTimeUTC", wanted, cnames)) {
    if ("BerlinDateTimeNoDST" %in% cnames) {
      #utcTime <- berlinWinterTimeToUTC(as.character(x$BerlinDateTimeNoDST))
      #x$DateTimeUTC <- utcTime$charUTC
      x$DateTimeUTC <- kwb.datetime::berlinNormalTimeToUTC(as.character(x$BerlinDateTimeNoDST))
    }
  }
  x
}

# addTotalVolumeAndMaxQ --------------------------------------------------------

#' Add Total Volume and Max Q
#' 
#' @param qValues vector of discharge values given in L/s
#' @param events event information as retrieved by \code{kwb.event::hsEvents}
#' @param eventnr integer vector of same length as \emph{qValues} giving the
#'   number of the event to which the Q value belongs, as returned by
#'   \code{kwb.event::hsEventNumber}.
#' @param digitsV number of decimal places for V in m3
#' @param digitsMaxQ number of decimal places for max. Q in L/s
#'
#' @return \emph{events} with columns \emph{V_m3} and \emph{maxQ_L_s} added
#' 
#' @export
#' 
addTotalVolumeAndMaxQ <- function(
  qValues, events, eventnr, digitsV = 3, digitsMaxQ = 3
)
{
  # calculate total volume and max flow per event
  myby <- list(eventnr=eventnr)
  qsum <- stats::aggregate(qValues, by=myby, FUN=sum)
  qmax <- stats::aggregate(qValues, by=myby, FUN=max)

  signalWidth <- kwb.event::hsSigWidth(events)
  cat(sprintf(
    "A signal width of %d seconds was deduced from the event list.\n",
    signalWidth
  ))
  
  events$V_m3 <- round(qsum$x / 1000 * signalWidth, digitsV)
  events$maxQ_L_s <- round(qmax$x, digitsMaxQ)

  events
}

# reformatEvents ---------------------------------------------------------------

#' Reformat Event List
#' 
#' Reformat event list: convert to minutes and rename columns
#' 
#' @param events event list as retrieved by \code{kwb.event::hsEvents}
#' 
#' @return \emph{events} with \emph{tBeg} renamed \emph{Ereignisbeginn_UTC},
#'   \emph{tEnd} renamed \emph{Ereignisende_UTC}, \emph{dur} renamed
#'   \emph{Dauer_min}, \emph{pBefore} renamed \emph{Pause_davor_min} and
#'   \emph{pAfter} renamed \emph{Pause_danach_min} and original columns
#'   \emph{iBeg} and \emph{iEnd} removed
#' 
#' @export
#' 
reformatEvents <- function(events)
{
  events <- kwb.event::hsEventsToUnit(events, tUnit = "min")
  
  events <- kwb.utils::hsRenameColumns(events, list(
    tBeg = "Ereignisbeginn_UTC",
    tEnd = "Ereignisende_UTC",
    dur = "Dauer_min",
    pBefore = "Pause_davor_min",
    pAfter = "Pause_danach_min"
  ))
  
  cbind(Nr = 1:nrow(events), events[, -(1:2)])
}

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

# dswtdir ----------------------------------------------------------------------

#' Path to Subfolder "DSWT" in tempdir()
#' 
#' Path to subfolder "DSWT" in tempdir(). If the subfolder does not yet exist
#'   it is created
#' 
#' @return path to subfolder "DSWT" in tempdir()
#' @export
dswtdir <- function()
{
  mydir <- file.path(tempdir(), "dswt")

  if (!file.exists(mydir)) {

    dir.create(mydir)
    cat(sprintf("directory \"%s\" created.\n", mydir))
  }

  mydir
}

# validate_HQ_relationships ----------------------------------------------------

#' Validate H-Q-Relationships
#' 
#' Validate HQ relationships for DN = 150 and DN = 300
#' 
#' @export
#' 
validate_HQ_relationships <- function()
{
  for (DN in c(150, 300)) {
    cat(sprintf("DN = %d\n", DN))
    validate_HQ_relationship(DN)
    hq <- H_Q_Table(DN)
    hq$calc <- round(H_to_Q(hq$H_m, DN), 3)
    print(hq)
  }
}

# validate_HQ_relationship -----------------------------------------------------

#' validate_HQ_relationship
#' 
#' Validate HQ relationship by plotting Q versus H from manufacturer table
#'   as points and the regression line given by the derived formula
#' @keywords internal
validate_HQ_relationship <- function(DN)
{
  hq <- H_Q_Table(DN)

  graphics::plot(
    hq$H_m, hq$Q_L_s, xlab = "H in m", ylab = "Q in L/s", 
    main = paste("DN", DN, sep=" = ")
  )
  
  h <- seq(0, max(hq$Q_L_s), by = 0.01)
  graphics::lines(h, H_to_Q(h, DN = DN), col = "blue")
}

# convertQUnits ----------------------------------------------------------------

#' Convert Q in L/h to L/s, m3/s and m3/h
#' 
#' @param hq data frame containing a column \emph{Q_L_h} containing flows in L/h
#' 
#' @return data frame with columns \emph{H_m, Q_L_s, Q_m3_s, Q_L_h, Q_m3_h}
#' 
convertQUnits <- function(hq)
{
  hq$Q_L_s  <- round(hq$Q_L_h / 3600, 3)
  hq$Q_m3_s <- round(hq$Q_L_h / 3600000, 6)
  hq$Q_m3_h <- round(hq$Q_L_h / 1000, 3)
  
  hq[, c("H_m", "Q_L_s", "Q_m3_s", "Q_L_h", "Q_m3_h")]
}

# H_Q_Table --------------------------------------------------------------------

#' H-Q-Relationship Given by Manufacturer
#' 
#' @param DN DN in mm, must be one of 150, 300
#' 
#' @return data frame with columns \emph{H_m, Q_L_s, Q_m3_s, Q_L_h, Q_m3_h}
#' 
H_Q_Table <- function(DN)
{
  if (DN == 150) {
    
    hq <- as.data.frame(.H_Q_Matrix_DN_150())
    
  } else if (DN == 300) {
    
    hq <- as.data.frame(.H_Q_Matrix_DN_300())
    
  } else {
    
    .stopWithNoSuchDN()
  }
  
  names(hq) <- c("H_m", "Q_L_h")
  convertQUnits(hq)
}

# Q_to_H -----------------------------------------------------------------------

#' Back-Calculate Height Above Flume From Discharge
#' 
#' back-calculates height H above flume from discharge Q.
#'   Q = a * H^b <=> H = (Q/a)^(1/b) with a and be retrieved from linear
#'   regression between log(H) and log(Q) with H and Q values taken from
#'   manufacturer's table
#' 
#' @param Q discharge Q in height above flume (in which unit?)
#' @param DN DN in mm, must be one of 150, 300
#' 
Q_to_H <- function(Q, DN)
{
  regressionCoefficients <- .regressionCoefficientsForDN(DN = DN)

  (Q/regressionCoefficients$a)^(1 / regressionCoefficients$b)
}

# .wantedButNotAvailable -------------------------------------------------------
.wantedButNotAvailable <- function(cname, wanted, available) {
    cname %in% wanted & !(cname %in% available)
}

# .H_Q_Matrix_DN_150 -----------------------------------------------------------
.H_Q_Matrix_DN_150 <- function() {
  matrix(ncol = 2, byrow = TRUE, c(
    0.006, 47.54, 
    0.012, 183.68, 
    0.018, 404.99, 
    0.025, 768.5, 
    0.032, 1243.66, 
    0.038, 1738.75, 
    0.044, 2314.16, 
    0.050, 2969.28, 
    0.057, 3833.68, 
    0.063, 4659.87, 
    0.069, 5564.37,
    0.075, 6546.81, 
    0.082, 7791.07, 
    0.088, 8941.31, 
    0.094, 10168.56, 
    0.100, 11472.56, 
    0.107, 13090.57, 
    0.113, 14560.06, 
    0.119, 16105.59, 
    0.125, 17726.98, 
    0.132, 19714.21, 
    0.138, 21499.3, 
    0.144, 23359.69,
    0.150, 25295.2, 
    0.157, 27648.05, 
    0.163, 29745.82, 
    0.169, 31918.26, 
    0.176, 34546.96, 
    0.184, 37675.15, 
    0.191, 40520.54, 
    0.198, 43466.76, 
    0.205, 46513.63, 
    0.212, 49660.96, 
    0.219, 52908.59, 
    0.227, 56778.56
  ))
}

# .H_Q_Matrix_DN_300 -----------------------------------------------------------
.H_Q_Matrix_DN_300 <- function() {
  matrix(ncol = 2, byrow = TRUE, c(
    0.013, 329.11, 
    0.026, 1271.6, 
    0.039, 2803.69, 
    0.052, 4913.15, 
    0.065, 7591.62, 
    0.078, 10832.73, 
    0.091, 14631.35, 
    0.104, 18983.16, 
    0.117, 23884.49, 
    0.130, 29332.1, 
    0.143, 35323.11, 
    0.156, 41854.93, 
    0.169, 48925.21, 
    0.182, 56531.8, 
    0.195, 64672.72, 
    0.208, 73346.11, 
    0.221, 82550.28, 
    0.234, 92283.6, 
    0.247, 102544.57, 
    0.260, 113331.76, 
    0.273, 124643.82, 
    0.286, 136479.48, 
    0.299, 148837.52, 
    0.312, 161716.77, 
    0.325, 175116.13, 
    0.338, 189034.54, 
    0.351, 203470.98, 
    0.364, 218424.48, 
    0.377, 233894.09, 
    0.390, 249878.9, 
    0.403, 266378.06, 
    0.416, 283390.7, 
    0.429, 300916.03, 
    0.442, 318953.25, 
    0.457, 339846.98
  ))
}

# .siteNameToDN ----------------------------------------------------------------

#' Find DN for Given Site
#' 
#' @param sitename name of monitoring site
#' @export
.siteNameToDN <- function(sitename) {
  
  if (kwb.utils::isNullOrEmpty(sitename)) {
    stop("No sitename given!")
  }
  
  DNs <- list(T = 300, C = 150)
  
  sitegroupCode <- substr(sitename, 1, 1)
  
  if (!(sitegroupCode %in% names(DNs))) {
    stop("No DN given for sitename: ", sitename)
  }
  
  DN <- DNs[[sitegroupCode]]
  
  cat("DN at site", sitename, ":", DN, "\n")
  
  DN
}
