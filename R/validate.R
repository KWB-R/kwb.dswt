# formatLevelFileInfo ----------------------------------------------------------

#' Format Level File Info
#' @keywords internal
formatLevelFileInfo <- function(levelFileInfo, new.format = NULL)
{
  if (is.null(new.format)) {
    new.format <- "%d.%m.%y %H:%M"
  }
  
  for (columnName in c("min", "first", "last", "max")) {
    formatted <- kwb.datetime::reformatTimestamp(
      levelFileInfo[[columnName]],
      old.format="%Y-%m-%d %H:%M:%S",
      new.format=new.format
    )
    levelFileInfo[[columnName]] <- formatted
  }
  
  # set min to "" if min == first
  levelFileInfo$min[levelFileInfo$min == levelFileInfo$first] <- ""
  
  # set max to "" if max == last
  levelFileInfo$max[levelFileInfo$max == levelFileInfo$last] <- ""
  
  levelFileInfo
}

# getLevelFilesInfo ------------------------------------------------------------

#' Get Level Files Info
#' @keywords internal
getLevelFilesInfo <- function(levelDataFiles, new.format = NULL)
{
  if (is.null(new.format)) {
    new.format <- "%d.%m.%y %H:%M"
  }
  
  result <- NULL
  
  for (levelDataFile in levelDataFiles) {
    result <- rbind(result, getLevelFileInfo(levelDataFile))
  }
  
  formatLevelFileInfo(result, new.format)
}

# getLevelFilesInfo2 -----------------------------------------------------------

#' Get Level Files Info 2
#' 
#' @param levelData data frame with columns \emph{myDateTime} (character),
#'   \emph{file}, \emph{row}, as returned by \code{\link{readAllLevelFiles}}
#'   
#' @return data frame with columns \code{file},\code{rows}, \code{min}, 
#'   \code{first}, \code{last}, \code{max}
#' 
#' @export
#' 
getLevelFilesInfo2 <- function(levelData)
{
  levelData$myDateTime <- kwb.datetime::hsToPosix(levelData$myDateTime)
  
  by <- list(levelData$file)
  
  data.frame(
    file  = unique(levelData$file),
    rows  = stats::aggregate(levelData$row, by = by, FUN = length)$x,
    min   = kwb.datetime::toUTC(stats::aggregate(levelData$myDateTime, by = by, FUN = min)$x),
    first = kwb.datetime::toUTC(stats::aggregate(levelData$myDateTime, by = by, FUN = utils::head, 1)$x),
    last  = kwb.datetime::toUTC(stats::aggregate(levelData$myDateTime, by = by, FUN = utils::tail, 1)$x),
    max   = kwb.datetime::toUTC(stats::aggregate(levelData$myDateTime, by = by, FUN = max)$x),
    stringsAsFactors = FALSE
  )
}

# getLevelFileInfo -------------------------------------------------------------

#' Get Level File Info
#' @keywords internal
getLevelFileInfo <- function(filePath, sep = ";", timeFormat = NULL)
{
  if (is.null(timeFormat)) {
    timeFormat <- c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M")
  }
  
  stopifnot(length(filePath) == 1)
  
  myData <- kwb.logger::readLogger_Ori_MLog(
    filePath, sep = sep, timeFormat = timeFormat, stopOnMissingColumns = FALSE
  )
    
  toLevelFileInfo(filePath, timestamps = myData$myDateTime)
}

# toLevelFileInfo --------------------------------------------------------------

#' Create Information on Water Level File
#' 
#' @param filePath path to water level file
#' @param timestamps vector of timestamps (read from the file?)
#' 
#' @export
toLevelFileInfo <- function(filePath, timestamps)
{
  data.frame(
    file = basename(filePath), 
    kB = round(file.info(filePath)$size / 1024, 1),
    rows = length(timestamps),
    getTimestampStatistics(timestamps),
    stringsAsFactors = FALSE
  )
}

# getTimestampStatistics -------------------------------------------------------

#' Get Timestamp Statistics
#' @keywords internal
getTimestampStatistics <- function(timestamps)
{  
  data.frame(
    min = min(timestamps), 
    first = utils::head(timestamps, 1), 
    last = utils::tail(timestamps, 1), 
    max = max(timestamps), 
    stringsAsFactors = FALSE
  )
}
