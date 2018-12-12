# formatLevelFileInfo ----------------------------------------------------------
formatLevelFileInfo <- function # formatLevelFileInfo
### formatLevelFileInfo
(
  levelFileInfo, new.format = NULL
)
{
  if (is.null(new.format)) {
    new.format <- "%d.%m.%y %H:%M"
  }
  
  for (columnName in c("min", "first", "last", "max")) {
    formatted <- reformatTimestamp(levelFileInfo[[columnName]],
                                   old.format="%Y-%m-%d %H:%M:%S",
                                   new.format=new.format)
    levelFileInfo[[columnName]] <- formatted
  }
  
  # set min to "" if min == first
  levelFileInfo$min[levelFileInfo$min == levelFileInfo$first] <- ""
  
  # set max to "" if max == last
  levelFileInfo$max[levelFileInfo$max == levelFileInfo$last] <- ""
  
  levelFileInfo
}

# getLevelFilesInfo ------------------------------------------------------------
getLevelFilesInfo <- function # getLevelFilesInfo
### getLevelFilesInfo
(
  levelDataFiles,
  new.format = NULL
)
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
getLevelFilesInfo2 <- function # getLevelFilesInfo2
### getLevelFilesInfo2
(
  levelData
  ### data frame with columns \emph{myDateTime} (character), \emph{file},
  ### \emph{row}, as returned by \code{\link{readAllLevelFiles}}
)
{
  levelData$myDateTime <- hsToPosix(levelData$myDateTime)
  
  by <- list(levelData$file)
  
  data.frame(
    file  = unique(levelData$file),
    rows  = aggregate(levelData$row, by = by, FUN = length)$x,
    min   = toUTC(aggregate(levelData$myDateTime, by = by, FUN = min)$x),
    first = toUTC(aggregate(levelData$myDateTime, by = by, FUN = head, 1)$x),
    last  = toUTC(aggregate(levelData$myDateTime, by = by, FUN = tail, 1)$x),
    max   = toUTC(aggregate(levelData$myDateTime, by = by, FUN = max)$x),
    stringsAsFactors = FALSE)
}

# getLevelFileInfo -------------------------------------------------------------
getLevelFileInfo <- function # getLevelFileInfo
### getLevelFileInfo
(
  filePath, 
  sep = ";",
  timeFormat = NULL
)
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
toLevelFileInfo <- function # toLevelFileInfo
### toLevelFileInfo
(
  filePath, timestamps
)
{
  data.frame(file = basename(filePath), 
             kB = round(file.info(filePath)$size/1024, 1),
             rows = length(timestamps),
             getTimestampStatistics(timestamps),
             stringsAsFactors = FALSE)
}

# getTimestampStatistics -------------------------------------------------------
getTimestampStatistics <- function # getTimestampStatistics
### getTimestampStatistics
(
  timestamps
)
{  
  data.frame(min = min(timestamps), 
             first = head(timestamps, 1), 
             last = tail(timestamps, 1), 
             max = max(timestamps), 
             stringsAsFactors = FALSE)
}
